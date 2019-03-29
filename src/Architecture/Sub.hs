module Architecture.Sub
    ( Sub
    , tcp
    , udp
    , none
    , Received (..)
    ) where

import Data.ByteString (ByteString)
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

none :: Sub msg
none = Sub []
