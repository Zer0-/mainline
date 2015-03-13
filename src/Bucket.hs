module Bucket where

import qualified Data.Map as Map

--                             +---------ID
--                             | +-------Bucket Size
--                             | |   +---Min Value
--                             | |   | +-Max value
--                             | |   | |
data RoutingTable a v = Bucket a Int a a (Map.Map a v)
    | Split (RoutingTable a v) (RoutingTable a v)

fits :: Ord a => a -> RoutingTable a v -> Bool
fits i (Split a b) = fits i a || fits i b
fits i (Bucket _ _ bmin bmax _) = bmin <= i && i < bmax

split :: Integral a => RoutingTable a v -> RoutingTable a v
split (Bucket bid bsize bmin bmax bmap) =
    Split (Bucket bid bsize bmin mid less) (Bucket bid bsize mid bmax more)
    where mid = fromInteger $ (toInteger bmin + toInteger bmax) `quot` 2
          (less, more) = Map.partitionWithKey (\k _ -> bmin <= k && k < mid) bmap
split rt = rt

insert :: (Ord a, Integral a) => a -> v -> RoutingTable a v -> RoutingTable a v
insert key value (Split a b)
    | fits key a = insert key value a
    | otherwise = insert key value b

insert key value (Bucket bid bsize bmin bmax bmap)
    | bmin <= key && key < bmax && Map.size bmap < bsize =
        Bucket bid bsize bmin bmax (Map.insert key value bmap)
    | bmin <= key && key < bmax && fits bid bucket = split $ Bucket bid bsize bmin bmax (Map.insert key value bmap)
    | otherwise = bucket
        where bucket = Bucket bid bsize bmin bmax bmap
