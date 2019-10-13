{-# LANGUAGE NamedFieldPuns, DataKinds #-}

module Mainline.ResolveMagnet
    ( Model (..)
    , Msg (..)
    , update
    , subscriptions
    , chooseNextBlk
    ) where

import Prelude hiding (init)
import Control.Applicative (liftA2)
import Data.Word (Word8, Word32)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Strict (singleton)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Serialize (encode, decode)
import Data.Default (def)

import qualified Data.BEncode as B
import qualified Network.BitTorrent.Exchange.Message as BT
import qualified Network.BitTorrent.Address as BT
import qualified Data.Torrent as BTT

import Network.KRPC.Helpers (hexify)
import Network.KRPC.Types (CompactInfo(..), NodeID, InfoHash)
import Network.Octets (fromByteString, octToByteString)
import Architecture.Sub (Sub, Received(..))
import qualified Architecture.Sub as Sub
import qualified Architecture.Cmd as Cmd
import Mainline.Mainline (Cmd)

metadataBlocksize :: Int
metadataBlocksize = 16384

data Model
    = Off
    | Handshake
    | ExtensionHandshake
    | Downloading
        { metadataSize :: Int
        , extMetadataMsgid :: Word8
        , lastBlkRequested :: Int
        , blocks :: Map Int ByteString
        }

data Msg
    = DownloadInfo NodeID InfoHash CompactInfo
    | GotHandshake InfoHash CompactInfo (Either String BT.Handshake)
    | Got Int InfoHash CompactInfo (Either String BT.Message)


update :: Msg -> Model -> (Model, Cmd Msg)
update (DownloadInfo ourid t ci) Off =
    (Handshake, Cmd.batch [ logmsg, sendHandshake ])

    where
        logmsg = Cmd.log Cmd.DEBUG
            [ "Downloading information"
            , show $ hexify $ octToByteString t
            , "from", show ci
            ]

        sendHandshake = Cmd.sendTCP t ci (encode handshake)

        handshake = BT.Handshake
            def
            (BT.toCaps [ BT.ExtExtended ])
            (BTT.InfoHash $ octToByteString t)
            (BT.PeerId $ octToByteString ourid)

update (GotHandshake t ci handshake) Handshake =
    (ExtensionHandshake, Cmd.batch [ logmsg, sendCmd ])

    where
        logmsg = Cmd.log Cmd.DEBUG
            [ "Have handshake:", show handshake
            ,"Will send:", show eshake
            , show $ encode eshake
            ]

        sendCmd = Cmd.sendTCP t ci (encode eshake)

        -- TODO: check handshake for supporting BT.ExtExtended
        -- Or just keep going

        eshake :: BT.Message
        eshake = BT.Extended $ BT.EHandshake $
            BT.nullExtendedHandshake supportedExtensions

        supportedExtensions :: BT.ExtendedCaps
        supportedExtensions = BT.ExtendedCaps $ singleton BT.ExtMetadata 1

update
    ( Got firstBlk t ci
        ( Right
            ( BT.Extended
                ( BT.EHandshake
                    ( BT.ExtendedHandshake
                        { BT.ehsMetadataSize = msize
                        , BT.ehsCaps = exts
                        }
                    )
                )
            )
        )
    )
    ExtensionHandshake =
        case (liftA2 (,)) msize mExtmMsgId of
            Nothing -> (Off, errmsg)
            Just (size, msgid) ->
                ( Downloading size msgid 0 Map.empty
                , Cmd.batch [ logmsg, pieceReq msgid firstBlk t ci ]
                )
        where
            errmsg = Cmd.log Cmd.WARNING
                [ "Could not get size from handshake response." ]

            logmsg = Cmd.log Cmd.DEBUG
                [
                "Have size from extended handshake:"
                , show msize
                , "supports extensions:"
                , show exts
                ]

            mExtmMsgId = Map.lookup BT.ExtMetadata (BT.extendedCaps exts)

update
    ( Got _ t ci
        ( Right
            ( BT.Extended
                ( BT.EMetadata
                    _
                    ( BT.MetadataData
                        { BT.piece = (BTT.Piece blk blkbs)
                        --, BT.totalSize
                        }
                    )
                )
            )
        )
    )
    ( Downloading
        { metadataSize
        , extMetadataMsgid
        , lastBlkRequested
        , blocks
        }
    ) =
        case nextblk of
            Nothing -> (Off, logmsgs) -- TODO: Cmd.bounce some message
            (Just i) ->
                ( Downloading metadataSize extMetadataMsgid i blocks2
                , Cmd.batch [ logmsg, pieceReq extMetadataMsgid i t ci ]
                )

        where
            blocks2 :: Map Int ByteString
            blocks2 = Map.insert blk blkbs blocks

            nextblk = chooseNextBlk metadataSize lastBlkRequested blocks2

            logmsg = Cmd.log Cmd.DEBUG
                [ "got block" , show blk
                , "size:" , show $ BS.length blkbs
                ]

            logmsgs = Cmd.log Cmd.INFO
                [ "Have whole info dict"
                , show $ (B.decode (combineBlocks blocks2) :: Either String BTT.InfoDict)
                ]

update
    (Got _ _ _ (Right (BT.Available _)))
    (Downloading { metadataSize, extMetadataMsgid, lastBlkRequested, blocks }) =
        ( Downloading
            { metadataSize
            , extMetadataMsgid
            , lastBlkRequested
            , blocks
            }
        , logmsg
        )

        where
            logmsg = Cmd.log
                Cmd.DEBUG
                [ "Ignoring Available response from server" ]

update (GotHandshake _ _ have) _ = (Off, logmsg)
    where
        logmsg = Cmd.log
            Cmd.INFO
            [ "Got unknown response from server:"
            , show have
            ]

update _ Off = (Off, Cmd.none)


subscriptions :: InfoHash -> CompactInfo -> Model -> Sub Msg
subscriptions _ _ Off = Sub.none
subscriptions t ci Handshake =
    Sub.readTCP t ci numToRead ((GotHandshake t ci) . decode . bytes)

    where
        numToRead :: ByteString -> Int
        numToRead bs
            = 1  -- length prefix
            + 19 -- "BitTorrent protocol"
            + 8  -- reserved
            + 20 -- InfoHash
            + 20 -- NodeID
            - BS.length bs

subscriptions t ci _ =
    Sub.readTCP t ci numToRead ((Got 0 t ci) . decode . bytes)

    where
        numToRead :: ByteString -> Int
        numToRead bs
            | BS.length bs < 4 = 4
            | otherwise = (expectedLen (BS.take 4 bs)) - (BS.length bs) + 4

        expectedLen :: ByteString -> Int
        expectedLen b = fromIntegral ((fromByteString b) :: Word32)


numBlks :: Int -> Int
numBlks size =
    ceiling
    ((fromIntegral size / fromIntegral metadataBlocksize) :: Float)

chooseNextBlk :: Int -> Int -> Map Int a -> Maybe Int
chooseNextBlk metadataSize lastBlkRequested haveblks =
    if Map.size haveblks == nblks then Nothing
    else
        if lastBlkRequested >= nblks - 1 || Map.member nxt haveblks
        then Just $
            -- would it be better to use set complement here?
            head $ filter ((flip Map.notMember) haveblks) [0..nblks - 1]
        else Just nxt

    where
        nxt = lastBlkRequested + 1
        nblks = numBlks metadataSize

combineBlocks :: Map Int ByteString -> ByteString
combineBlocks = BS.concat . Map.elems

pieceReq :: Word8 -> Int -> InfoHash -> CompactInfo -> Cmd Msg
pieceReq msgid i t ci =
    Cmd.sendTCP
        t
        ci
        (encode (BT.Extended $ BT.EMetadata msgid (BT.MetadataRequest i)))


{-
main :: IO ()
main = simpleApp init update subscriptions
-}
