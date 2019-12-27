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
import Data.Time.Clock.POSIX (POSIXTime)
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

instance Show Model where
    show Off = "Off"
    show Handshake = "Handshake"
    show ExtensionHandshake = "ExtensionHandshake"
    show (Downloading _ _ _ _) = "Downloading"

data Msg
    = DownloadInfo NodeID InfoHash CompactInfo
    | GotHandshake InfoHash CompactInfo (Either String BT.Handshake)
    | Got POSIXTime Int InfoHash CompactInfo (Either String BT.Message)
    | Have POSIXTime InfoHash BTT.InfoDict
    | TCPError InfoHash CompactInfo


update :: Msg -> Model -> (Model, Cmd Msg)
update (DownloadInfo ourid t ci) Off =
    (Handshake, Cmd.batch [ logmsg, sendHandshake ])

    where
        logmsg = Cmd.log Cmd.DEBUG
            [ "Downloading information"
            , show $ hexify $ octToByteString t
            , "from", show ci
            ]

        sendHandshake = Cmd.sendTCP t ci (encode handshake) (TCPError t ci)

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
            , "from", show ci
            ,"Will send:", show eshake
            , show $ encode eshake
            ]

        sendCmd = Cmd.sendTCP t ci (encode eshake) (TCPError t ci)

        -- TODO: check handshake for supporting BT.ExtExtended
        -- Or just keep going

        eshake :: BT.Message
        eshake = BT.Extended $ BT.EHandshake $
            BT.nullExtendedHandshake supportedExtensions

        supportedExtensions :: BT.ExtendedCaps
        supportedExtensions = BT.ExtendedCaps $ singleton BT.ExtMetadata 1

update
    ( Got _ firstBlk t ci
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
                if size < 1 then (Off, errmsg2)
                else
                    ( Downloading size msgid 0 Map.empty
                    , Cmd.batch [ logmsg, pieceReq msgid firstBlk t ci ]
                    )
        where
            errmsg = Cmd.log Cmd.DEBUG
                [ "Could not get size from handshake response." ]

            errmsg2 = Cmd.log Cmd.INFO
                [ "Not starting info download for"
                , show t, "- size from extended handshake is zero." ]

            logmsg = Cmd.log Cmd.DEBUG
                [ "Have extended handshake from", show ci
                , "Have size:", show msize
                , "supports extensions:", show exts
                , "requesting piece", show firstBlk
                ]

            mExtmMsgId = Map.lookup BT.ExtMetadata (BT.extendedCaps exts)

update
    ( Got now _ t ci
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
            Nothing -> (Off, whenDone)
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


            -- case of Nothing

            decodeResult :: Either String BTT.InfoDict
            decodeResult = B.decode (combineBlocks blocks2)

            whenDone = case decodeResult of
                Left errmsg ->
                    Cmd.log Cmd.DEBUG
                        [ "Failed to fetch", prettyIH
                        , "Reason given:", errmsg
                        ]
                Right infodict ->
                    Cmd.batch
                    [ Cmd.log Cmd.DEBUG
                        [ prettyIH
                        , "info successfully downloaded"
                        ]
                    ,
                        let
                            calculated =
                                BTT.getInfoHash $ BTT.idInfoHash infodict
                            expected = octToByteString t
                        in
                            if calculated /= expected
                            then Cmd.log Cmd.WARNING
                                [ "Calculated infohash does not match expected!"
                                , "calculated:", hexify calculated
                                , "expected:", hexify expected
                                ]
                            else Cmd.bounce $ Have now t infodict
                    ]

            prettyIH = show $ hexify $ octToByteString t

update
    (Got _ _ _ _ (Right (BT.Available _)))
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
            Cmd.DEBUG
            [ "Got unknown response from server:"
            , show have
            ]

update _ Off = (Off, Cmd.none)
update (DownloadInfo _ _ _) _ = (Off, Cmd.none)
update (Got _ _ _ _ msg) _      = (Off, Cmd.log Cmd.DEBUG
    [ "Received unexpected message having never shook hands:", show msg ])


update (Have _ _ _) _  = (Off, Cmd.none)
update (TCPError _ _) _  = (Off, Cmd.none) -- handled in App

subscriptions :: InfoHash -> CompactInfo -> Model -> Sub Msg
subscriptions _ _ Off = Sub.none
subscriptions t ci Handshake =
    Sub.readTCP
        t
        ci
        (0 :: Int)
        numToRead
        ((GotHandshake t ci) . decode . bytes)
        (TCPError t ci)

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
    Sub.readTCP
        t
        ci
        (1 :: Int)
        numToRead
        mkMsg
        (TCPError t ci)

    where
        numToRead :: ByteString -> Int
        numToRead bs
            | BS.length bs < 4 = 4
            | otherwise = (expectedLen (BS.take 4 bs)) - (BS.length bs) + 4

        expectedLen :: ByteString -> Int
        expectedLen b = fromIntegral ((fromByteString b) :: Word32)

        mkMsg :: Received -> Msg
        mkMsg Received { bytes, time } = Got time 0 t ci (decode bytes)


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
        (TCPError t ci)


{-
main :: IO ()
main = simpleApp init update subscriptions
-}
