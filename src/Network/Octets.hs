module Network.Octets
    ( Octets      (..)
    , octToByteString
    , octToString
    , fromByteString
    ) where

import Data.List (foldl')
import Data.Bits (Bits, (.|.), shiftL, shiftR)
import Data.Word (Word8, Word16, Word32)
import Data.Digest.SHA1 (Word160 (Word160))
import qualified Data.ByteString as BS
import System.Endian (fromBE16, toBE16, fromBE32, toBE32)

import Network.KRPC.Helpers (extendListWith)

class Octets a where
    octets :: a -> [Word8]
    fromOctets :: [Word8] -> a


numFromOctets :: (Num a, Bits a) => [Word8] -> a
numFromOctets = foldl' accum 0
  where
    accum a o = (a `shiftL` 8) .|. fromIntegral o


instance Octets Word16 where
    octets w =
        [ fromIntegral (x `shiftR` 8)
        , fromIntegral x
        ] where x = toBE16 w
    fromOctets = fromBE16 . numFromOctets . (take 2) . (`extendListWith` 0)


instance Octets Word32 where
    octets w =
        [ fromIntegral (x `shiftR` 24)
        , fromIntegral (x `shiftR` 16)
        , fromIntegral (x `shiftR` 8)
        , fromIntegral x
        ] where x = toBE32 w
    fromOctets = fromBE32 . numFromOctets . (take 4) . (`extendListWith` 0)


instance Octets Word160 where
    octets (Word160 a1 a2 a3 a4 a5)
        = octets a1 ++ octets a2 ++ octets a3 ++ octets a4 ++ octets a5

    fromOctets bytes = Word160 a b c d e
        where a = fromOctets $ take 4 bytes
              b = fromOctets $ take 4 (drop 4 bytes)
              c = fromOctets $ take 4 (drop 8 bytes)
              d = fromOctets $ take 4 (drop 12 bytes)
              e = fromOctets $ take 4 (drop 16 bytes)




octToByteString :: (Octets a) => a -> BS.ByteString
octToByteString = BS.pack . octets


octToString :: (Octets a) => a -> String
octToString = show . octToByteString


fromByteString :: (Octets a) => BS.ByteString -> a
fromByteString = fromOctets . BS.unpack
