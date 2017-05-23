module Network.KRPC.Helpers
    ( extendListWith
    , stringunpack
    , stringpack
    , bd
    ) where

import Data.BEncode (BValue (..))
import Data.BEncode.BDict (BDictMap (..), singleton)

import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString as BS


extendListWith :: (Enum a) => [a] -> a -> [a]
extendListWith l a = l ++ [a, a..]


stringpack :: String -> BS.ByteString
stringpack = Char8.pack


stringunpack :: BS.ByteString -> String
stringunpack = Char8.unpack


bd :: String -> String -> BDictMap BValue
bd a b = singleton (stringpack a) (BString $ stringpack b)