{-# LANGUAGE NamedFieldPuns #-}

import Prelude hiding (init)
import Data.Word (Word32)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Hex (unhex, hex)
import Data.Map.Strict (singleton)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Serialize (encode, decode)
import Data.Default (def)
import Data.Maybe (fromJust)
import Data.List (sortOn)

import qualified Data.BEncode as B
import qualified Network.BitTorrent.Exchange.Message as BT
import qualified Network.BitTorrent.Address as BT
import qualified Data.Torrent as BTT

import Network.KRPC.Helpers (stringpack)
import Network.KRPC.Types (Port, CompactInfo(..))
import Network.Octets (fromOctets, fromByteString)
import Architecture.TEA (Config (..), run)
import Architecture.Sub (Sub, Received(..))
import qualified Architecture.Sub as Sub
import qualified Architecture.Cmd as Cmd
import Architecture.Cmd (Cmd)

import Debug.Trace (traceShow)

knownNodeHost :: Word32
knownNodeHost = fromOctets [ 192, 168, 4, 2 ]

knownNodePort :: Port
knownNodePort = 51413

knownNodeInfo :: CompactInfo
knownNodeInfo = CompactInfo knownNodeHost knownNodePort

testHash :: String
testHash = "139945d35fc1751c6bd144f0de7b5a124d09df79"

metadataBlocksize :: Int
metadataBlocksize = 16384

data Model
    = Off
    | Handshake
    | ExtensionHandshake
    | Downloading
        { metadataSize :: Int
        , lastBlkRequested :: Int
        , blocks :: Map Int ByteString
        }

data Msg
    = NewNodeID ByteString
    | Got (Either String BT.Message)

config :: Config Model Msg
config = Config init update subscriptions

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

update (Got handshake) Handshake = (ExtensionHandshake, Cmd.batch [ logmsg, sendCmd ])
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
                    ( BT.ExtendedHandshake { BT.ehsMetadataSize = msize }
                    )
                )
            )
        )
    )
    ExtensionHandshake =
        case msize of
            Nothing -> (Off, errmsg)
            Just size ->
                ( Downloading size 0 Map.empty
                , sendCmd
                )
        where
            errmsg = Cmd.log Cmd.WARNING
                [ "Could not get size from handshake response." ]

            sendCmd = Cmd.sendTCP knownNodeInfo (encode request)

            --TODO: check for zero size
            --request block 0
            request = BT.Extended $ BT.EMetadata 1 (BT.MetadataRequest 0)

            -- create Set to hold done and maybe in-progress pieces
            -- create last ix requested, increment it. If that is out of bounds
            -- perform a search where we get the first ix that isn't in done and isn't
            -- the last ix requested
            --
            -- calculate the number of total pieces
            --  -first we need to get the size of the info dict in bytes from
            --  the handshake response. Call it T ✓.
            --  cieling(T / 16384 bytes) = number of blocks
            --  blocks are indexed starting at zero

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
    (Downloading { metadataSize, lastBlkRequested, blocks }) =
        case nextblk of
            Nothing -> (Off, logmsgs)
            (Just i) -> (Downloading metadataSize i blocks2, mkSend i)

        where
            blocks2 :: Map Int ByteString
            blocks2 = Map.insert blk blkbs blocks

            nblks = numBlks metadataSize

            nextblk = chooseNextBlk lastBlkRequested nblks blocks2

            mkSend i = Cmd.sendTCP knownNodeInfo (encode (pieceReq i))

            logmsgs = Cmd.log Cmd.INFO
                [ "Have whole info dict"
                , show $ (B.decode (combineBlocks blocks2) :: Either String BTT.InfoDict)
                ]
        -- store piece in blocks
        -- if | blocks | = number of pieces then we're done, parse
        -- the info dict and print it, also hash it and compare with original
        -- info hash (must match)
        --
        -- if we're not done request the next piece

-- From bep_0009:
--
-- """
-- The metadata is handled in blocks of 16KiB (16384 Bytes). The metadata
-- blocks are indexed starting at 0. All blocks are 16KiB except the last block
-- which may be smaller.
--"""
--
--We actually have the total size of metadata in the extended handshake
--
--There are now three types of possible messages, all using the extension proto
-- (which means they all start with a length, 1 byte of "20" and 1 byte of
-- whatever number the response ExtensionHandshake specified ExtMetadata to be and then
-- a bencoded dict)
-- We need to send a request for a piece (and the number of pieces and which piece
--      - Create a Bitfield to keep track of pieces?
--      - maybe use
--          haveNone :: PieceCount -> Bitfield
--      - or just store the completed ones in a set and check against that.
--              - 1MB / 16k = 66 pieces, small sets for each torrent in progress
--
-- Create response for this request
--
-- -Torrent files are between 25Kb and 1.1Mb
--
-- is for us to figure out) and await a response (or reject) and do this for all
-- pieces.


update _ Off = (Off, Cmd.none)


subscriptions :: Model -> Sub Msg
subscriptions Off = Sub.none
subscriptions Handshake = Sub.readTCP knownNodeInfo numToRead recvToMsg
    where
        numToRead :: ByteString -> Int
        numToRead bs
            = 1  -- length prefix
            + 19 -- "BitTorrent protocol"
            + 8  -- reserved
            + 20 -- InfoHash
            + 20 -- NodeID
            - BS.length bs

subscriptions _ = Sub.readTCP knownNodeInfo numToRead recvToMsg
    where
        numToRead :: ByteString -> Int
        numToRead bs
            | BS.length bs < 4 = 4
            | otherwise = (expectedLen (BS.take 4 bs)) - (BS.length bs) + 4

        expectedLen :: ByteString -> Int
        expectedLen b = fromIntegral ((fromByteString b) :: Word32)

recvToMsg :: Received -> Msg
recvToMsg r = Got $ decode $ (traceShow (BS.length $ bytes r) (bytes r))

numBlks :: Int -> Int
numBlks size = ceiling ((fromIntegral size) / (fromIntegral metadataBlocksize))

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
combineBlocks blks = BS.concat $ map snd (sortOn fst (Map.assocs blks))

pieceReq :: Int -> BT.Message
pieceReq i = BT.Extended $ BT.EMetadata 1 (BT.MetadataRequest i)

main :: IO ()
main = run config
