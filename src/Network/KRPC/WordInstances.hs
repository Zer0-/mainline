module Network.KRPC.WordInstances where

import Prelude hiding   (toInteger)
import Data.Function    (on)
import Data.Digest.SHA1 (Word160, toInteger)

    --{-# MINIMAL compare #-}
instance Ord Word160 where
    compare = on compare toInteger
