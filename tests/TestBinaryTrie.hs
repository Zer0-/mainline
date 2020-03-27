module TestBinaryTrie
    ( trieHasInsertedKey
    , deletedNotInTrie
    , closestReturnsSomething
    , closestIsXorClosest
    ) where

import Prelude hiding (lookup)
import Data.Maybe (isJust, isNothing, fromJust)
import Data.List (sortBy)
import Mainline.RoutingTable (orderingf)
import Data.BinaryTrie
    ( fromList
    , lookup
    , delete
    , closest
    , Trie
    )

positive :: [(Integer, a)] -> [(Integer, a)]
positive = map (\(x, y) -> (abs x, y))

trieHasInsertedKey
    :: [(Integer, String)]
    -> (Integer, String)
    -> [(Integer, String)]
    -> Bool
trieHasInsertedKey itemsA (k, val) itemsB = isJust $ lookup trie key
    where
        trie :: Trie String
        trie = fromList $ (positive itemsA) ++ [(key, val)] ++ (positive itemsB)

        key = abs k


deletedNotInTrie
    :: [(Integer, String)]
    -> (Integer, String)
    -> [(Integer, String)]
    -> Bool
deletedNotInTrie itemsA (k, val) itemsB =
    isNothing $ lookup (delete trie key) key
    where
        key = abs k
        trie =
            fromList $ (positive itemsA) ++ [(key, val)] ++ (positive itemsB)

closestReturnsSomething
    :: [(Integer, String)]
    -> Integer
    -> Bool
closestReturnsSomething items k = null items || (isJust $ closest trie key)
    where
        trie = fromList (positive items)
        key = abs k

closestIsXorClosest
    :: [Integer]
    -> Integer
    -> Bool
closestIsXorClosest ints k = null ints ||
    (  (head $ sortBy (orderingf key) positiveInts)
    == (fromJust $ closest trie key)
    )

    where
        positiveInts = map abs ints
        items = map (\x -> (x, x)) positiveInts
        trie = fromList items
        key = abs k
