module Architecture.Sub
    ( Sub
    , tcp
    ) where

import qualified Data.ByteString as BS
import Network.KRPC.Types (Port)

import Architecture.Internal.Sub
    ( TSub (..)
    , Sub (..)
    )


tcp :: Port -> (BS.ByteString -> msg) -> Sub msg
tcp port m = Sub [ TCP port ]
