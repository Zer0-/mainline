module Network.KRPC.WordInstances () where

import Data.Function    (on)
import Data.Digest.SHA1 (Word160, lift2)
import qualified Data.Digest.SHA1 as W160

instance Ord Word160 where
    compare = on compare toInteger

-- Can use encode Data.Binary to convert Integer to ByteString, then use fromOctets

quot1 :: Word160 -> Word160 -> Word160
quot1 = lift2 quot

rem1 :: Word160 -> Word160 -> Word160
rem1 = lift2 rem

instance Integral Word160 where
    quot = quot1
    rem = rem1
    div = lift2 div
    mod = lift2 mod
    quotRem x y = (quot1 x y, rem1 x y)
    toInteger = W160.toInteger

instance Enum Word160 where
    toEnum = undefined
    fromEnum = undefined

instance Real Word160 where
    toRational = undefined

instance Num Word160 where
    (+) = lift2 (+)
    (-) = lift2 (-)
    (*) = lift2 (*)
    signum = undefined
    fromInteger = undefined
    abs = id
