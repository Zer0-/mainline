{-
 - This module is an almost direct translation to haskell the contents of
 - the paper "Fast Mergeable Integer Maps" by Chris Okasaki and Andy Gill
 - <http://citeseer.ist.psu.edu/okasaki98fast.html>
 -}


import Data.Bits((.&.), (.|.), xor, complement)

import Prelude hiding (lookup)
import Data.Foldable (foldl')

import Debug.Trace (trace)

data Trie a
    = Empty
    | Leaf Integer a
    | Branch Integer Integer (Trie a) (Trie a)
    deriving Show

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

{-
join :: Integer -> Integer -> Trie a -> Integer -> Integer -> Trie a -> Trie a
join p0 m0 t0 p1 m1 t1 = if zeroBit p0 m
    then Branch p3 m t0 t1
    else Branch p3 m t1 t0

    where
        m = branchingBit p0 m0 p1 m1
        p3 = mask p0 m
-}
join :: Show a => Integer -> Trie a -> Integer -> Trie a -> Trie a
join p0 t0 p1 t1 = if zeroBit p0 m
    then Branch p3 m t0 t1
    else Branch p3 m t1 t0

    where
        m = branchingBit p0 p1
        p3 = mask p0 m


insert :: Show a => Integer -> a -> Trie a -> Trie a
insert k x t = ins t
    where
        ins Empty = Leaf k x
        ins (Leaf j y) =
            if j == k then Leaf k x
            else join k (Leaf k x) j (Leaf j y)
        ins (Branch p m t0 t1) =
            if matchPrefix k p m then
                if zeroBit k m
                then Branch p m (ins t0) t1
                else Branch p m t0 (ins t1)
            else join k (Leaf k x) p (Branch p m t0 t1)


{- for little endian trees
branchingBit :: Integer -> Integer -> Integer
branchingBit p0 p1 = lowestBit (p0 `xor` p1)
-}

-- for big endian trees
{-
branchingBit :: Integer -> Integer -> Integer -> Integer -> Integer
branchingBit p0 m0 p1 m1 = highestBit (p0 `xor` p1) (max 1 (2 * max m0 m1))
-}

branchingBit :: Integer -> Integer -> Integer
branchingBit p0 p1 = highestBit (p0 `xor` p1)

twosComplement :: Integer -> Integer
twosComplement x = (complement x) + 1

lowestBit :: Integer -> Integer
lowestBit x = x .&. (twosComplement x)

{-
highestBit :: Integer -> Integer -> Integer
highestBit x m = highb x_
    where
        x_ = x .&. (complement (m - 1))
        highb y =
            let n = lowestBit y
            in if y == n then n else highb (y - n)
-}

highestBit :: Integer -> Integer
highestBit x = if x == m then m else highestBit (x - m)
    where
        m = lowestBit x

fromList :: (Foldable t, Show a) => t (Integer, a) -> Trie a
fromList = foldl' (flip (uncurry insert)) Empty

{-
example :: Trie String
example = Branch 0 1 (Leaf 4 "Hi") (Leaf 3 "You")
-}

example2 :: Trie String
--example2 = fromList [(7, "asdf"), (13, "yay"), (1, "cake")]
example2 = fromList [(4, "yay"), (7, "foot"), (13, "wtf"), (1, "cake")]

main :: IO ()
main = do
    mapM_ (putStrLn . show . (lookup example2)) [0..15]
    putStrLn ""
    mapM_ (putStrLn . show . (lookup (delete example2 13))) [0..15]
    putStrLn ""
    mapM_ (\i -> putStrLn $ "xor 4:" ++ show (i `xor` 4) ++ " xor 7:" ++ show (i `xor` 7) ++ " xor 13:" ++ show (i `xor` 13) ++ " " ++ (show . (closest example2)) i) [0..15]
    putStrLn ""
    print example2

{-
 - Quickchecks:
 - Any trie, insert one node, always has that node
 -}
