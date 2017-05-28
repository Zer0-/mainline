module Mainline.Mainline
    ( Outbound
    , ServerState (..)
    , NotImplemented (..)
    , serverHandler
    ) where

import Network.KRPC (KPacket)
import Network.KRPC.Types (CompactInfo, Message)
import qualified Data.Map as Map
import Data.ByteString (ByteString)

data NotImplemented = NotImplemented

data ServerState = ServerState
    { transactionState :: Map.Map ByteString NotImplemented
    }

type Outbound = (CompactInfo, Either KPacket (Message, NotImplemented))

serverHandler :: ServerState -> CompactInfo -> KPacket -> (ServerState, [Outbound])
serverHandler = undefined
