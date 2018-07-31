module Architecture.Sub
    ( Sub
    , tcp
    , none
    ) where

import Data.ByteString (ByteString)
import Network.KRPC.Types (Port)

import Architecture.Internal.Sub
    ( TSub (..)
    , Sub (..)
    )


tcp :: Port -> (ByteString -> msg) -> Sub msg
tcp port h = Sub [ TCP port h ]

none :: Sub msg
none = Sub []
