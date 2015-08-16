import Bucket
import KRPC

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework (defaultMain, testGroup, Test)

import Data.List (nub)
import Data.Word (Word32)
import qualified Data.Map as Map

import Data.ByteString (ByteString)

import Data.BEncode
import Data.BEncode.BDict

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

fmt_decodeErr :: ByteString -> Integer -> String -> Bool
fmt_decodeErr tid code msg = fromBEncode bval == (Right $ KPacket tid (Error code msg))
    where bval = BDict $
                    (singleton (stringpack "t") (BString tid))
                    `union` bd "y" "e"
                    `union` (singleton (stringpack "e") (BList [BInteger code, BString (stringpack msg)]))

tests :: [Test]
tests = [
        testGroup "QuickCheck Bucket" [
            testProperty "Fits into bucket and split"              prop_fits,
            testProperty "Always store things in boundless bucket" prop_size
            ]
        ]

main :: IO ()
main = do
    defaultMain tests
    putStrLn $ show $ (toBEncode testKPacket)
    putStrLn $ show $ (encode testKPacket)
