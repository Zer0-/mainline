{-
 - This module is an almost direct translation to haskell the contents of
 - the paper "Fast Mergeable Integer Maps" by Chris Okasaki and Andy Gill
 - <http://citeseer.ist.psu.edu/okasaki98fast.html>
 -}

module Data.BinaryTrie
    ( lookup
    , empty
    , closest
    , delete
    , insert
    , fromList
    , Trie
    ) where


import Data.Bits((.&.), (.|.), xor, complement)

import Prelude hiding (lookup)
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


delete :: Trie a -> Integer -> Trie a
delete Empty _ = Empty
delete (Leaf j x) k = if j == k then Empty else Leaf j x
delete (Branch p m a b) k = if zeroBit k m
    then br p m (delete a k) b
    else br p m a (delete b k)

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

insert :: Show a => Integer -> a -> Trie a -> Trie a
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

twosComplement :: Integer -> Integer
twosComplement x = (complement x) + 1

lowestBit :: Integer -> Integer
lowestBit x = x .&. (twosComplement x)

highestBit :: Integer -> Integer -> Integer
highestBit x m = highb x_
    where
        x_ = x .&. (complement (m - 1))
        highb y =
            let n = lowestBit y
            in if y == n then n else highb (y - n)

fromList :: (Foldable t, Show a) => t (Integer, a) -> Trie a
fromList = foldl' (flip (uncurry insert)) Empty
