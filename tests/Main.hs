import Bucket

import qualified Data.Map as Map
import Data.List (nub)
import Data.Word (Word32)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework (defaultMain, testGroup, Test)

prop_fits :: Word32 -> Int -> Word32 -> Word32 -> Word32 -> Bool
prop_fits bid size bmin bmax value =
    fits value bucket == fits value (split bucket)
    where bucket = Bucket bid size bmin bmax Map.empty

prop_size :: Int -> [Int] -> Bool
prop_size bid xs = bsize (foldr (\j b -> insert j j b) bucket (nub xs)) == len
    where len = length $ nub xs
          bucket = Bucket bid len minBound maxBound Map.empty
          bsize (Split a b) = bsize a + bsize b
          bsize (Bucket _ _ _ _ bMap) = Map.size bMap

tests :: [Test]
tests = [
        testGroup "QuickCheck Bucket" [
            testProperty "Fits into bucket and split"              prop_fits,
            testProperty "Always store things in boundless bucket" prop_size
            ]
        ]

main :: IO ()
main = defaultMain tests
