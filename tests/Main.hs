import Bucket
import qualified Data.Map as Map

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework (defaultMain, testGroup, Test)

prop_fits :: Int -> Int -> Int -> Int -> Int -> Bool
prop_fits bid size bmin bmax value =
    fits value bucket == fits value (split bucket)
    where bucket = Bucket bid size bmin bmax Map.empty

tests :: [Test]
tests = [
        testGroup "QuickCheck Bucket" [
            testProperty "Fits into bucket and split" prop_fits
            ]
        ]

main :: IO ()
main = defaultMain tests
