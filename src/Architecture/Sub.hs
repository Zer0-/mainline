module Architecture.Sub
    ( Sub
    , tcp
    , udp
    , none
    , Received (..)
    ) where

import Data.ByteString (ByteString)
import Network.Socket (SockAddr)
import Network.KRPC.Types (Port)

import Architecture.Internal.Sub
    ( TSub (..)
    , Sub (..)
    , Received (..)
    )

tcp :: Port -> (ByteString -> msg) -> Sub msg
tcp port h = Sub [ TCP port h ]

udp :: Port -> (SockAddr -> Received -> msg) -> Sub msg
udp port h = Sub [ UDP port h ]

none :: Sub msg
none = Sub []
