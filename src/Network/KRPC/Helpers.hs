module Network.KRPC.Helpers
    ( extendListWith
    , stringpack
    , bd
    , hexify
    ) where

import Data.BEncode (BValue (..))
import Data.BEncode.BDict (BDictMap (..), singleton)

import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString as BS


extendListWith :: (Enum a) => [a] -> a -> [a]
extendListWith l a = l ++ [a, a..]


stringpack :: String -> BS.ByteString
stringpack = Char8.pack


bd :: String -> String -> BDictMap BValue
bd a b = singleton (stringpack a) (BString $ stringpack b)

hexify :: BS.ByteString -> String
hexify = show . decodeUtf8 . toStrict . toLazyByteString . byteStringHex
