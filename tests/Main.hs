import Bucket
import KRPC

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework (defaultMain, testGroup, Test)

import Data.List (nub)
import Data.Word (Word32, Word8)
import qualified Data.Map as Map

import Data.ByteString (pack)
import qualified Data.ByteString as BS

import Data.BEncode
import Data.BEncode.BDict hiding (map)

--import Debug.Trace

prop_fits :: Word32 -> Int -> Word32 -> Word32 -> Word32 -> Bool
prop_fits bid size bmin bmax value =
    fits value bucket == fits value (split bucket)
    where bucket = Bucket bid size bmin bmax Map.empty

prop_size :: Word32 -> [Word32] -> Bool
prop_size bid xs = bsize (foldr (\j b -> insert j j b) bucket (nub xs)) == len
    where len = length $ nub xs
          bucket = Bucket bid len minBound maxBound Map.empty
          bsize (Split a b) = bsize a + bsize b
          bsize b = Map.size $ bucketData b

fmt_decodeErr :: [Word8] -> Integer -> String -> Bool
fmt_decodeErr i code msg = fromBEncode bval == (Right $ KPacket tid (Error code msg))
    where bval = BDict $ (singleton (stringpack "t") (BString tid))
                            `union` bd "y" "e"
                            `union` (singleton (stringpack "e") (BList [BInteger code, BString (stringpack msg)]))
          tid = pack i

fmt_decodePingQuery :: [Word8] -> [Word8] -> Bool
fmt_decodePingQuery i n = fromBEncode bval == (Right $ KPacket tid (Query (fromByteString nid) Ping))
    where bval = BDict $ singleton bs_a (BDict (singleton bs_id (BString nid)))
                            `union` (singleton bs_t (BString tid))
                            `union` bd "q" "ping"
                            `union` bd "y" "q"
          tid = pack i
          nid = pack n

fmt_decodePingResponse :: [Word8] -> [Word8] -> Bool
fmt_decodePingResponse i n = fromBEncode bval == (Right $ KPacket tid (Response (fromByteString nid) Ping))
    where bval = BDict $ singleton bs_r (BDict (singleton bs_id (BString nid)))
                            `union` (singleton bs_t (BString tid))
                            `union` bd "y" "r"
          tid = pack i
          nid = pack n

fmt_decodeFindNodeQuery :: [Word8] -> [Word8] -> [Word8] -> Bool
fmt_decodeFindNodeQuery i n t = fromBEncode bval == (Right $ KPacket tid (Query (fromByteString nid) (FindNode (fromByteString target))))
    where bval = BDict $
                    singleton bs_a
                        (BDict (singleton bs_id (BString nid)
                                    `union` singleton (stringpack "target") (BString target)))
                    `union` (singleton bs_t (BString tid))
                    `union` bd "q" "find_node"
                    `union` bd "y" "q"
          tid = pack i
          nid = pack n
          target = pack t

fmt_decodeNodesResponse :: [Word8] -> [Word8] -> [Word8] -> Bool
fmt_decodeNodesResponse t i ns
    = fromBEncode bval == expected
    where tid = pack t
          nid = pack i
          bval = BDict $
            singleton bs_r (BDict $
                singleton bs_id (BString nid)
                `union` singleton (stringpack "nodes") (BString $ pack ns))
            `union` bd "y" "r"
            `union` singleton bs_t (BString tid)
          expected :: Result KPacket
          expected = (Right $
                       KPacket tid $
                           Response
                               (fromByteString nid)
                               (Nodes $ mkNodes ns))
          mkNodes :: [Word8] -> [NodeInfo]
          mkNodes [] = []
          mkNodes xs = fromOctets (take 166 xs) : mkNodes (drop 166 xs)

fmt_decodeAskPeersQuery :: [Word8] -> [Word8] -> [Word8] -> Bool
fmt_decodeAskPeersQuery t i info
    = fromBEncode bval == (Right $
                          KPacket tid $ Query (fromByteString nid) (AskPeers $ fromOctets info))
    where tid = pack t
          nid = pack i
          bval = BDict $
            singleton bs_a (BDict $
                singleton bs_id (BString nid)
                `union` singleton (stringpack "info_hash") (BString (pack info)))
            `union` bd "y" "q"
            `union` bd "q" "get_peers"
            `union` singleton bs_t (BString tid)

tests :: [Test]
tests = [
        testGroup "QuickCheck Bucket" [
            testProperty "Fits into bucket and split"              prop_fits,
            testProperty "Always store things in boundless bucket" prop_size
            ],
        testGroup "KRPC Sanity" [
            testProperty "decode Error Message"                    fmt_decodeErr,
            testProperty "decode Ping Query Message"               fmt_decodePingQuery,
            testProperty "decode Ping Response Message"            fmt_decodePingResponse,
            testProperty "decode FindNode Query Message"           fmt_decodeFindNodeQuery,
            testProperty "decode Nodes Response Message"           fmt_decodeNodesResponse,
            testProperty "decode AskPeers Query Message"           fmt_decodeAskPeersQuery
            ]
        ]

main :: IO ()
main = do
    defaultMain tests
