import qualified Data.ByteString as BS
import Crypto.Random (newGenIO, genBytes)
import Crypto.Random.DRBG (CtrDRBG)
import Crypto.Util (bs2i)
import Data.List (foldl')
import Criterion.Main (defaultMain, bgroup, env, whnf, bench)
import qualified Data.Map as Map

import Data.BinaryTrie
    ( Trie
    , insert
    , empty
    , delete
    )

infohashMaxBytes :: Int
infohashMaxBytes = 20

-- stolen from here (thanks!):
-- https://hackage.haskell.org/package/hw-prim-0.6.3.0/docs/src/HaskellWorks.Data.ByteString.html#chunkedBy
chunkedBy :: Int -> BS.ByteString -> [BS.ByteString]
chunkedBy n bs = if BS.length bs == 0
  then []
  else case BS.splitAt n bs of
    (as, zs) -> as : chunkedBy n zs


insertDeleteBPT :: [ Integer ] -> Int -> Trie Integer
insertDeleteBPT ints n = deleteManyBPT ints trie n
    where
        trie = insertManyBPT ints n


deleteManyBPT :: [ Integer ] -> Trie Integer -> Int -> Trie Integer
deleteManyBPT ints trie n = foldl' (\t i -> delete t i) trie (take n ints)


insertManyBPT :: [ Integer ] -> Int -> Trie Integer
insertManyBPT ints n = foldl' (\t i -> insert i i t) empty (take n ints)


insertDeleteMap :: [ Integer ] -> Int -> Map.Map Integer Integer
insertDeleteMap ints n = deleteManyMap ints m n
    where
        m = insertManyMap ints n


insertManyMap :: [ Integer ] -> Int -> Map.Map Integer Integer
insertManyMap ints n = foldl' (\m i -> Map.insert i i m) Map.empty (take n ints)

deleteManyMap :: [ Integer ] -> Map.Map Integer Integer -> Int -> Map.Map Integer Integer
deleteManyMap ints m n = foldl' (\m_ i -> Map.delete i m_) m (take n ints)


makeRandomIntegers :: Int -> Int -> IO [ Integer ]
makeRandomIntegers count bs_per_int = do
    g <- newGenIO :: IO CtrDRBG

    case genBytes (count * bs_per_int) g of
        Left err -> error $ show err
        Right (result, _) ->
            return $ map bs2i (chunkedBy bs_per_int result)


createTrieEnv :: Int -> IO [Integer]
createTrieEnv n = makeRandomIntegers n infohashMaxBytes


main :: IO ()
main = defaultMain
    [
        bgroup "Binary Particia Trie"
            [ env
                (createTrieEnv 10000)
                ( \ints -> bgroup "all"
                    [ {- bench "10 BPT" $ whnf (insertDeleteBPT ints) 10
                    , bench "10 Map" $ whnf (insertDeleteMap ints) 10

                    , bench "100 BPT" $ whnf (insertDeleteBPT ints) 100
                    , bench "100 Map" $ whnf (insertDeleteMap ints) 100

                    , bench "1000 BPT" $ whnf (insertDeleteBPT ints) 1000
                    , bench "1000 Map" $ whnf (insertDeleteMap ints) 1000

                    
                    ,-} bench "10000 BPT" $ whnf (insertDeleteBPT ints) 10000
                    --, bench "10000 Map" $ whnf (insertDeleteMap ints) 10000
                    ]
                )
            ]
    ]
