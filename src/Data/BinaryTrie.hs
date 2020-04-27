{-
 - This module is an almost direct translation to haskell the contents of
 - the paper "Fast Mergeable Integer Maps" by Chris Okasaki and Andy Gill
 - <http://citeseer.ist.psu.edu/okasaki98fast.html>
 -}

module Data.BinaryTrie
    ( lookup
    , empty
    , closest
    , nclosest
    , modify
    , delete
    , insert
    , fromList
    , elems
    , Trie
    ) where


import Data.Bits((.&.), (.|.), xor, complement)

import Prelude hiding (lookup, foldr)
import Data.Foldable (foldl')

data Trie a
    = Empty
    | Leaf Integer a
    | Branch Integer Integer (Trie a) (Trie a)
    deriving Show

empty :: Trie a
empty = Empty

br :: Integer -> Integer -> Trie a -> Trie a -> Trie a
br _ _ Empty t = t
br _ _ t Empty = t
br p m t0 t1 = Branch p m t0 t1

lookup :: Trie a -> Integer -> Maybe a
lookup Empty _ = Nothing
lookup (Leaf j x) k = if j == k then Just x else Nothing
lookup (Branch _ m a b) k = if zeroBit k m then lookup a k else lookup b k


closest :: Trie a -> Integer -> Maybe a
closest Empty _ = Nothing
closest (Leaf _ x) _ = Just x
closest (Branch _ m a b) k =
    if zeroBit k m then closest a k else closest b k


nclosest :: Int -> Trie a -> Integer -> [a]
nclosest n t k = (fst $ nclosest_ n t k) []

nclosest_ :: Int -> Trie a -> Integer -> ([a] -> [a], Int)
nclosest_ 0 _ _ = (id, 0)
nclosest_ _ Empty _ = (id, 0)
nclosest_ _ (Leaf _ x) _ = ((x :), 1)
nclosest_ c (Branch _ m a b) k = (as . bs, d + e)
    where
        (a_, b_) = if zeroBit k m then (a, b) else (b, a)
        (as, d) = nclosest_ c a_ k
        (bs, e) = nclosest_ (c - d) b_ k

modify :: (a -> Maybe a) -> Trie a -> Integer -> Trie a
modify _ Empty _ = Empty
modify f (Leaf j x) k =
    if j == k
    then (maybe Empty (Leaf j) (f x))
    else Leaf j x
modify f (Branch p m a b) k = if zeroBit k m
    then br p m (modify f a k) b
    else br p m a (modify f b k)

delete :: Trie a -> Integer -> Trie a
delete = modify (const Nothing)

zeroBit :: Integer -> Integer -> Bool
zeroBit k m = (k .&. m) == 0

{-
 - -- for little endian trie
mask :: Integer -> Integer -> Integer
mask k m = k .&. (m - 1)
-}

-- for big endian trie
mask :: Integer -> Integer -> Integer
mask k m = (k .|. (m - 1)) .&. (complement m)

matchPrefix :: Integer -> Integer -> Integer -> Bool
matchPrefix k p m = (mask k m) == p

join :: Integer -> Integer -> Trie a -> Integer -> Integer -> Trie a -> Trie a
join p0 m0 t0 p1 m1 t1 = if zeroBit p0 m
    then Branch p3 m t0 t1
    else Branch p3 m t1 t0

    where
        m = branchingBit p0 m0 p1 m1
        p3 = mask p0 m

insert :: Integer -> a -> Trie a -> Trie a
insert k x t = ins t
    where
        ins Empty = Leaf k x
        ins (Leaf j y) =
            if j == k then Leaf k x
            else join k 0 (Leaf k x) j 0 (Leaf j y)
        ins (Branch p m t0 t1) =
            if matchPrefix k p m then
                if zeroBit k m
                then Branch p m (ins t0) t1
                else Branch p m t0 (ins t1)
            else join k 0 (Leaf k x) p m (Branch p m t0 t1)


{- for little endian trees
branchingBit :: Integer -> Integer -> Integer
branchingBit p0 p1 = lowestBit (p0 `xor` p1)
-}

-- for big endian trees
branchingBit :: Integer -> Integer -> Integer -> Integer -> Integer
branchingBit p0 m0 p1 m1 = highestBit (p0 `xor` p1) (max 1 (2 * max m0 m1))


lowestBit :: Integer -> Integer
lowestBit x = x .&. (-x)


highestBit :: Integer -> Integer -> Integer
highestBit x m = highb x_
    where
        x_ = x .&. (complement (m - 1))
        highb y =
            let n = lowestBit $! y
            in if y == n then n else highb $! (y - n)


fromList :: Foldable t => t (Integer, a) -> Trie a
fromList = foldl' (flip (uncurry insert)) Empty

foldr :: (a -> b -> b) -> b -> Trie a -> b
foldr _ c Empty = c
foldr f c (Leaf _ x) = f x c
foldr f c (Branch _ _ a b) = foldr f (foldr f c b) a

elems :: Trie a -> [a]
elems = foldr (:) []
