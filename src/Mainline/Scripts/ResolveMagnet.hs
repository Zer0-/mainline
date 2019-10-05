{-# LANGUAGE NamedFieldPuns, DataKinds #-}

import Prelude hiding (init)
import Control.Applicative (liftA2)
import Data.Word (Word8, Word32)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Hex (unhex, hex)
import Data.Map.Strict (singleton)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Serialize (encode, decode)
import Data.Default (def)
import Data.Maybe (fromJust)

import qualified Data.BEncode as B
import qualified Network.BitTorrent.Exchange.Message as BT
import qualified Network.BitTorrent.Address as BT
import qualified Data.Torrent as BTT

import Network.KRPC.Helpers (stringpack)
import Network.KRPC.Types (Port, CompactInfo(..))
import Network.Octets (fromOctets, fromByteString)
import Architecture.TEA (simpleApp)
import Architecture.Sub (Sub, Received(..))
import qualified Architecture.Sub as Sub
import qualified Architecture.Cmd as Cmd

type Cmd msg = Cmd.Cmd msg '[]

knownNodeHost :: Word32
knownNodeHost = fromOctets [ 192, 168, 4, 2 ]

knownNodePort :: Port
knownNodePort = 51413

knownNodeInfo :: CompactInfo
knownNodeInfo = CompactInfo knownNodeHost knownNodePort

testHash :: String
testHash = "039945d35fc1751c6bd144f0de7b5a124d09df79"

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
    = NewNodeID ByteString
    | GotHandshake (Either String BT.Handshake)
    | Got (Either String BT.Message)

init :: (Model, Cmd Msg)
init = (Off, Cmd.randomBytes 20 NewNodeID)

update :: Msg -> Model -> (Model, Cmd Msg)
update (NewNodeID bs) Off = (Handshake, Cmd.batch [ logmsg, sendHandshake ])
    where
        logmsg = Cmd.log Cmd.DEBUG [ "Hello World", show $ hex bs ]
        sendHandshake = Cmd.sendTCP knownNodeInfo (encode handshake)
        handshake = BT.Handshake
            def
            (BT.toCaps [ BT.ExtExtended ])
            (BTT.InfoHash $ fromJust $ unhex $ stringpack testHash)
            (BT.PeerId bs)

update (GotHandshake handshake) Handshake =
    (ExtensionHandshake, Cmd.batch [ logmsg, sendCmd ])
    where
        logmsg = Cmd.log Cmd.DEBUG
            [ "Have handshake:", show handshake
            ,"Will send:", show eshake
            , show $ encode eshake
            ]

        sendCmd = Cmd.sendTCP knownNodeInfo (encode eshake)

        -- TODO: check handshake for supporting BT.ExtExtended
        -- Or just keep going

        eshake :: BT.Message
        eshake = BT.Extended $ BT.EHandshake $
            BT.nullExtendedHandshake supportedExtensions

        supportedExtensions :: BT.ExtendedCaps
        supportedExtensions = BT.ExtendedCaps $ singleton BT.ExtMetadata 1

update
    ( Got
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
                , Cmd.batch [ logmsg, mkSendCmd msgid ]
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
            mkSendCmd msgid = pieceReq msgid 0 knownNodeInfo

update
    ( Got
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
            Nothing -> (Off, logmsgs)
            (Just i) ->
                ( Downloading metadataSize extMetadataMsgid i blocks2
                , Cmd.batch [ logmsg, pieceReq extMetadataMsgid i knownNodeInfo ]
                )

        where
            blocks2 :: Map Int ByteString
            blocks2 = Map.insert blk blkbs blocks

            nblks = numBlks metadataSize

            nextblk = chooseNextBlk lastBlkRequested nblks blocks2

            logmsg = Cmd.log Cmd.DEBUG
                [ "got block" , show blk
                , "size:" , show $ BS.length blkbs
                ]

            logmsgs = Cmd.log Cmd.INFO
                [ "Have whole info dict"
                , show $ (B.decode (combineBlocks blocks2) :: Either String BTT.InfoDict)
                ]

update
    (Got (Right (BT.Available _)))
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

update (GotHandshake have) _ = (Off, logmsg)
    where
        logmsg = Cmd.log
            Cmd.INFO
            [ "Got unknown response from server:"
            , show have
            ]

update _ Off = (Off, Cmd.none)


subscriptions :: Model -> Sub Msg
subscriptions Off = Sub.none
subscriptions Handshake =
    Sub.readTCP knownNodeInfo numToRead (GotHandshake . decode . bytes)

    where
        numToRead :: ByteString -> Int
        numToRead bs
            = 1  -- length prefix
            + 19 -- "BitTorrent protocol"
            + 8  -- reserved
            + 20 -- InfoHash
            + 20 -- NodeID
            - BS.length bs

subscriptions _ = Sub.readTCP knownNodeInfo numToRead (Got . decode . bytes)
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
chooseNextBlk lastBlkRequested nblks haveblks =
    if Map.size haveblks == nblks then Nothing
    else
        if lastBlkRequested >= nblks - 1 || Map.member nxt haveblks
        then Just $
            head $ filter ((flip Map.notMember) haveblks) [0..nblks - 1]
        else Just nxt

    where
        nxt = lastBlkRequested + 1

combineBlocks :: Map Int ByteString -> ByteString
combineBlocks = BS.concat . Map.elems

pieceReq :: Word8 -> Int -> CompactInfo -> Cmd Msg
pieceReq msgid i ci =
    Cmd.sendTCP
    ci
    (encode (BT.Extended $ BT.EMetadata msgid (BT.MetadataRequest i)))


main :: IO ()
main = simpleApp init update subscriptions
