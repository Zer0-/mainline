import Bucket
import KRPC

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework (defaultMain, testGroup, Test)

import Data.List (nub)
import Data.Word (Word32, Word8)
import qualified Data.Map as Map

import Data.ByteString (pack)

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

fmt_decodeNodeResponse :: [Word8] -> [Word8] -> [Word8] -> Bool
fmt_decodeNodeResponse t i n
        = fromBEncode bval == (Right $
                               KPacket tid $
                                   Response
                                       (fromByteString nid)
                                       (Node $ fromByteString ni))
    where tid = pack t
          nid = pack i
          ni = pack n
          bval = BDict $
            singleton bs_r (BDict $
                singleton bs_id (BString nid)
                `union` singleton (stringpack "nodes") (BString ni))
            `union` bd "y" "r"
            `union` (singleton bs_t (BString tid))

fmt_decodeNodesResponse :: [Word8] -> [Word8] -> [[Word8]] -> Bool
fmt_decodeNodesResponse t i ns
        = fromBEncode bval == (Right $
                               KPacket tid $
                                   Response
                                       (fromByteString nid)
                                       (Nodes $ map fromOctets ns))
    where tid = pack t
          nid = pack i
          bval = BDict $
            singleton bs_r (BDict $
                singleton bs_id (BString nid)
                `union` singleton (stringpack "nodes") (BList (map (\x -> BString (pack x)) ns)))
            `union` bd "y" "r"
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
            testProperty "decode Node Response Message"            fmt_decodeNodeResponse,
            testProperty "decode Nodes Response Message"           fmt_decodeNodesResponse
            ]
        ]

{-
testbval = BDict $
                (singleton (stringpack "t") (BString (stringpack "")))
                `union` bd "y" "e"
                `union` (singleton (stringpack "e") (BList [BInteger 0, BString (stringpack "")]))

testKPacket :: KPacket
testKPacket = KPacket (stringpack "transaction") (Query (Word160 0 0 0 0 1337) Ping)

testKPacket :: KPacket
testKPacket = KPacket (stringpack "transaction2") (
    Nodes [ NodeInfo (Word160 1 1 1 1 1337) (CompactInfo 192 8080)
          , NodeInfo (Word160 2 2 2 2 1338) (CompactInfo 168 9999)
          ]
    )
-}

main :: IO ()
main = do
    defaultMain tests
{-
    putStrLn $ show $ testbval
    putStrLn $ show $ ((fromBEncode testbval) :: Result KPacket)
-}
