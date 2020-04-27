module TestBinaryTrie
    ( trieHasInsertedKey
    , deletedNotInTrie
    , closestReturnsSomething
    , closestIsXorClosest
    , nclosestReturnsSomething
    , nclosestIsXorClosest
    ) where

import Prelude hiding (lookup)
import qualified Data.Set as Set
import Data.Maybe (isJust, isNothing, fromJust)
import Data.List (sortBy)
import Mainline.RoutingTable (orderingf)
import Data.BinaryTrie
    ( fromList
    , lookup
    , delete
    , closest
    , nclosest
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

nclosestReturnsSomething
    :: [(Integer, String)]
    -> Integer
    -> Int
    -> Bool
nclosestReturnsSomething items_ k n_ =
    null items_ || length (nclosest n trie key) == min len n

    where
        n = abs n_
        items = map (\(i, v) -> (abs i, v)) items_
        trie = fromList items
        key = abs k
        len = Set.size $ Set.fromList (map (abs . fst) items)


nclosestIsXorClosest
    :: [Integer]
    -> Integer
    -> Int
    -> Bool
nclosestIsXorClosest items_ k n_
    = nclosest n trie key == (take n $ sortBy (orderingf key) items)

    where
        n = abs n_
        items = Set.toList $ Set.fromList $ map abs items_
        trie = fromList $ map (\x -> (x, x)) items
        key = abs k
