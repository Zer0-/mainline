{-# LANGUAGE NamedFieldPuns #-}

module Mainline.Bucket where

import qualified Data.Set as Set

data Bucket a = Bucket
    { bucketId   :: a
    , bucketSize :: Int
    , minKey     :: a
    , maxKey     :: a
    , bucketData :: Set.Set a
    }
    | Split (Bucket a) (Bucket a)


fits :: Ord a => a -> Bucket a -> Bool
fits i (Split a b) = fits i a || fits i b
fits i (Bucket { minKey, maxKey }) = minKey <= i && i < maxKey


split :: Integral a => Bucket a -> Bucket a
split (Bucket bid bsize bmin bmax bset) =
    Split (Bucket bid bsize bmin mid less) (Bucket bid bsize mid bmax more)
    where mid = if bmin < bmax
                    then bmin + ((bmax - bmin) `quot` 2)
                    else bmax + ((bmin - bmax) `quot` 2)
          (less, more) = Set.partition (\k -> bmin <= k && k < mid) bset

split rt = rt


find :: Ord a => a -> Bucket a -> Bucket a
find value (Split a b)
    | fits value a = find value a
    | otherwise = find value b

find _ b = b


getId :: Bucket a -> a
getId (Split a _) = getId a
getId b = bucketId b


insert :: (Ord a, Integral a) => a -> Bucket a -> Bucket a
insert value (Split a b)
    | fits value a = Split (insert value a) b
    | otherwise    = Split a (insert value b)

insert value bucket
    | Set.size bset < bucketSize bucket =
        bucket { bucketData = Set.insert value bset }
    | fits (bucketId bucket) bucket =
        insert value $ split bucket
    | otherwise = bucket
    where
        bset = bucketData bucket

delete :: Ord a => a -> Bucket a -> Bucket a
delete val (Split a b)
    | fits val a =
        let new = delete val a in
            case new of
                Bucket {} ->
                    if Set.null (bucketData new) then b
                    else Split new b
                _ -> Split new b
    | otherwise =
        let new = delete val b in
            case new of
                Bucket {} ->
                    if Set.null (bucketData new) then a
                    else Split a new
                _ -> Split a new

delete val bucket =
    bucket { bucketData = Set.delete val (bucketData bucket) }


-- This can return true given a non-root Bucket and value
-- that belongs outside of it's range.
willInsert :: (Ord a) => a -> Bucket a -> Bool
willInsert value (Split a b)
    | fits value a = willInsert value a
    | otherwise    = willInsert value b

willInsert _ bucket
    | Set.size bset < bucketSize bucket = True
    | fits (bucketId bucket) bucket     = True
    | otherwise                         = False
    where
        bset = bucketData bucket
