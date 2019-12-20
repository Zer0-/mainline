module Architecture.Sub
    ( Sub
    , Received (..)
    , readTCP
    , udp
    , timer
    , none
    , batch
    , up
    ) where

import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import Data.Time.Clock.POSIX (POSIXTime)

import Network.KRPC.Types (Port, CompactInfo)

import Architecture.Internal.Types
    ( TSub (..)
    , Sub (..)
    )
import Architecture.Internal.Sub (Received (..), mapTSub)

readTCP :: (Hashable t, Hashable u)
        => t
        -> CompactInfo
        -> u
        -> (ByteString -> Int)
        -> (Received -> msg)
        -> msg
        -> Sub msg
readTCP t ci fkey g h e = Sub [ TCPClient t ci fkey g h e ]

udp :: Port -> (CompactInfo -> Received -> msg) -> msg -> Sub msg
udp port h e = Sub [ UDP port h e ]

timer :: Int -> (POSIXTime -> msg) -> Sub msg
timer dt h = Sub [ Timer dt h ]

none :: Sub msg
none = Sub []

batch :: [ Sub msg ] -> Sub msg
batch subs = Sub $ subs >>= (\(Sub tsubs) -> tsubs)

up :: (msg0 -> msg1) -> Sub msg0 -> Sub msg1
up f (Sub ss) = Sub $ map (mapTSub f) ss
