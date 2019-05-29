module Architecture.Sub
    ( Sub
    , tcp
    , udp
    , timer
    , none
    , batch
    , Received (..)
    ) where

import Data.ByteString (ByteString)
import Data.Time.Clock.POSIX (POSIXTime)

import Network.KRPC.Types (Port, CompactInfo)

import Architecture.Internal.Sub
    ( TSub (..)
    , Sub (..)
    , Received (..)
    )

tcp :: Port -> (ByteString -> msg) -> Sub msg
tcp port h = Sub [ TCP port h ]

udp :: Port -> (CompactInfo -> Received -> msg) -> Sub msg
udp port h = Sub [ UDP port h ]

timer :: Int -> (POSIXTime -> msg) -> Sub msg
timer dt h = Sub [ Timer dt h ]

none :: Sub msg
none = Sub []

batch :: [ Sub msg ] -> Sub msg
batch subs = Sub $ subs >>= (\(Sub tsubs) -> tsubs)
