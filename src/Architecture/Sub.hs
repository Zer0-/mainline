module Architecture.Sub
    ( Sub
    , tcp
    , udp
    , none
    ) where

import Data.ByteString (ByteString)
import Network.Socket (SockAddr)
import Network.KRPC.Types (Port)

import Architecture.Internal.Sub
    ( TSub (..)
    , Sub (..)
    )


tcp :: Port -> (ByteString -> msg) -> Sub msg
tcp port h = Sub [ TCP port h ]

udp :: Port -> (SockAddr -> ByteString -> msg) -> Sub msg
udp port h = Sub [ UDP port h ]

none :: Sub msg
none = Sub []
