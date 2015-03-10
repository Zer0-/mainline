module Bucket where

import qualified Data.Map as Map

--                           +-------------ID
--                           |   +---------Bucket Size
--                           |   |   +-----Min distance
--                           |   |   |   +-Max distance
--                           |   |   |   |
data RoutingTable v = Bucket Int Int Int Int (Map.Map Int v)
    | Split (RoutingTable v) (RoutingTable v)

bucketMin :: RoutingTable v -> Int
bucketMin (Split a _) = bucketMin a
bucketMin (Bucket _ _ bmin _ _) = bmin

bucketMax :: RoutingTable v -> Int
bucketMax (Split _ b) = bucketMax b
bucketMax (Bucket _ _ _ bmax _) = bmax

bucketID :: RoutingTable v -> Int
bucketID (Split a _) = bucketID a
bucketID (Bucket bid _ _ _ _) = bid

fits :: Int -> RoutingTable v -> Bool
fits i (Split a b) = (bucketMin a <= i) && (i < bucketMax b)
fits i (Bucket _ _ bmin bmax _) = bmin <= i && i < bmax

split :: RoutingTable v -> RoutingTable v
split (Bucket bid bsize bmin bmax bmap) =
    Split (Bucket bid bsize bmin mid less) (Bucket bid bsize mid bmax more)
    where mid = (bmin + bmax) `quot` 2
          (less, more) = Map.partitionWithKey (\k _ -> bmin <= k && k < mid) bmap
split rt = rt

insert :: Int -> v -> RoutingTable v -> RoutingTable v
insert key value (Split a b)
    | fits key a = insert key value a
    | otherwise = insert key value b

insert key value (Bucket bid bsize bmin bmax bmap)
    | bmin <= key && key < bmax && Map.size bmap < bsize =
        Bucket bid bsize bmin bmax (Map.insert key value bmap)
    | bmin <= key && key < bmax && fits bid bucket = split $ Bucket bid bsize bmin bmax (Map.insert key value bmap)
    | otherwise = bucket
        where bucket = Bucket bid bsize bmin bmax bmap
