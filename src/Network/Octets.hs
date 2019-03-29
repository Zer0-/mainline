module Network.Octets
    ( Octets      (..)
    , octToByteString
    , fromByteString
    ) where

import Data.List (foldl')
import Data.Bits (Bits, (.|.), shiftL, shiftR)
import Data.Word (Word8, Word16, Word32)
import Data.Digest.SHA1 (Word160 (Word160))
import qualified Data.ByteString as BS

--import System.Endian (fromBE16)

-- Everything in here should parse words as being in Network Byte Order
-- And store words in Host Byte Order

class Octets a where
    octets :: a -> [Word8]
    fromOctets :: [Word8] -> a


numFromOctets :: (Num a, Bits a) => [Word8] -> a
numFromOctets = foldl' accum 0
  where
    accum a o = (a `shiftL` 8) .|. fromIntegral o


instance Octets Word16 where
    octets w =
        [ fromIntegral (w `shiftR` 8)
        , fromIntegral w
        ]

    fromOctets = numFromOctets . (take 2)


instance Octets Word32 where
    octets w =
        [ fromIntegral (w `shiftR` 24)
        , fromIntegral (w `shiftR` 16)
        , fromIntegral (w `shiftR` 8)
        , fromIntegral w
        ]

    fromOctets = numFromOctets . (take 4)


instance Octets Word160 where
    octets (Word160 a1 a2 a3 a4 a5) =
        octets a5 ++ octets a4 ++ octets a3 ++ octets a2 ++ octets a1

    fromOctets bytes = Word160 a b c d e
        where
            a = fromOctets $ take 4 (drop 16 bytes)
            b = fromOctets $ take 4 (drop 12 bytes)
            c = fromOctets $ take 4 (drop 8 bytes)
            d = fromOctets $ take 4 (drop 4 bytes)
            e = fromOctets $ take 4 bytes


octToByteString :: (Octets a) => a -> BS.ByteString
octToByteString = BS.pack . octets


fromByteString :: (Octets a) => BS.ByteString -> a
fromByteString = fromOctets . BS.unpack
