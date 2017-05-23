module Mainline.Mainline
    ( Outbound
    , ServerState
    ) where

import Network.KRPC (KPacket)
import Network.KRPC.Types (CompactInfo)

type Outbound = [(KPacket, CompactInfo)]

data ServerState = Null

serverHandler :: ServerState -> Either String KPacket -> (Outbound, ServerState)
serverHandler s (Left e) = ([], s)
