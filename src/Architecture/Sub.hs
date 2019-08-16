module Architecture.Sub
    ( Sub
    , readTCP
    , udp
    , timer
    , none
    , batch
    , Received (..)
    ) where

import Data.ByteString (ByteString)
import Data.Time.Clock.POSIX (POSIXTime)

import Network.KRPC.Types (Port, CompactInfo)

import Architecture.Internal.Types
    ( TSub (..)
    , Sub (..)
    )
import Architecture.Internal.Sub (Received (..))

readTCP :: CompactInfo -> (ByteString -> Int) -> (Received -> msg) -> Sub msg
readTCP ci g h = Sub [ TCPClient ci g h ]

udp :: Port -> (CompactInfo -> Received -> msg) -> Sub msg
udp port h = Sub [ UDP port h ]

timer :: Int -> (POSIXTime -> msg) -> Sub msg
timer dt h = Sub [ Timer dt h ]

none :: Sub msg
none = Sub []

batch :: [ Sub msg ] -> Sub msg
batch subs = Sub $ subs >>= (\(Sub tsubs) -> tsubs)
