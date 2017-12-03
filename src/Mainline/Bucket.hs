{-# LANGUAGE NamedFieldPuns #-}

module Mainline.Bucket where

import qualified Data.Set as Set

data RoutingTable a = Bucket
    { bucketId   :: a
    , bucketSize :: Int
    , minKey     :: a
    , maxKey     :: a
    , bucketData :: Set.Set a
    }
    | Split (RoutingTable a) (RoutingTable a)

fits :: Ord a => a -> RoutingTable a -> Bool
fits i (Split a b) = fits i a || fits i b
fits i (Bucket { minKey, maxKey }) = minKey <= i && i < maxKey

split :: Integral a => RoutingTable a -> RoutingTable a
split (Bucket bid bsize bmin bmax bset) =
    Split (Bucket bid bsize bmin mid less) (Bucket bid bsize mid bmax more)
    where mid = if bmin < bmax
                    then bmin + ((bmax - bmin) `quot` 2)
                    else bmax + ((bmin - bmax) `quot` 2)
          (less, more) = Set.partition (\k -> bmin <= k && k < mid) bset
split rt = rt

insert :: (Ord a, Integral a) => a -> RoutingTable a -> RoutingTable a
insert value (Split a b)
    | fits value a = Split (insert value a) b
    | otherwise = Split a (insert value b)

insert value bucket
    | Set.size bset < bucketSize bucket =
        bucket { bucketData = Set.insert value bset }
    | fits (bucketId bucket) bucket =
        insert value $ split bucket
    | otherwise = bucket
    where
        bset = bucketData bucket
