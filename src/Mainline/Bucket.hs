{-# LANGUAGE NamedFieldPuns #-}

module Mainline.Bucket where

import qualified Data.Map as Map

data RoutingTable a v = Bucket
    { bucketId   :: a
    , bucketSize :: Int
    , minVal     :: a
    , maxVal     :: a
    , bucketData :: Map.Map a v
    }
    | Split (RoutingTable a v) (RoutingTable a v)

fits :: Ord a => a -> RoutingTable a v -> Bool
fits i (Split a b) = fits i a || fits i b
fits i (Bucket { minVal, maxVal }) = minVal <= i && i < maxVal

split :: Integral a => RoutingTable a v -> RoutingTable a v
split (Bucket bid bsize bmin bmax bmap) =
    Split (Bucket bid bsize bmin mid less) (Bucket bid bsize mid bmax more)
    where mid = if bmin < bmax
                    then bmin + ((bmax - bmin) `quot` 2)
                    else bmax + ((bmin - bmax) `quot` 2)
          (less, more) = Map.partitionWithKey (\k _ -> bmin <= k && k < mid) bmap
split rt = rt

insert :: (Ord a, Integral a) => a -> v -> RoutingTable a v -> RoutingTable a v
insert key value (Split a b)
    | fits key a = Split (insert key value a) b
    | otherwise = Split a (insert key value b)

insert key value bucket
    | Map.size bmap < bucketSize bucket =
        bucket { bucketData = Map.insert key value bmap }
    | fits (bucketId bucket) bucket =
        insert key value $ split bucket
    | otherwise = bucket
    where
        bmap = bucketData bucket
