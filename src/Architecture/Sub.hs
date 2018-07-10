module Architecture.Sub
    ( Sub
    , tcp
    , none
    ) where

import qualified Data.ByteString as BS
import Network.KRPC.Types (Port)

import Architecture.Internal.Sub
    ( TSub (..)
    , Sub (..)
    )


tcp :: Port -> (BS.ByteString -> msg) -> Sub msg
tcp port h = Sub [ TCP port h ]

none :: Sub msg
none = Sub []
