import Bucket
import KRPC

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework (defaultMain, testGroup, Test)

import Data.List (nub)
import Data.Word (Word32, Word8)
import qualified Data.Map as Map

import Data.ByteString (pack)

import Data.BEncode
import Data.BEncode.BDict

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
    where bval = BDict $
                    (singleton (stringpack "t") (BString tid))
                    `union` bd "y" "e"
                    `union` (singleton (stringpack "e") (BList [BInteger code, BString (stringpack msg)]))
          tid = pack i

fmt_decodePing :: [Word8] -> [Word8] -> Bool
fmt_decodePing i n = fromBEncode bval == (Right $ KPacket tid (Query (parseNodeID nid) Ping))
    where bval = BDict $
                    singleton bs_a (BDict (singleton bs_id (BString nid)))
                    `union` (singleton bs_t (BString tid))
                    `union` bd "q" "ping"
                    `union` bd "y" "q"
          tid = pack i
          nid = pack n

tests :: [Test]
tests = [
        testGroup "QuickCheck Bucket" [
            testProperty "Fits into bucket and split"              prop_fits,
            testProperty "Always store things in boundless bucket" prop_size
            ],
        testGroup "KRPC Sanity" [
            testProperty "decode error message"                    fmt_decodeErr,
            testProperty "decode Ping  message"                    fmt_decodePing
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
