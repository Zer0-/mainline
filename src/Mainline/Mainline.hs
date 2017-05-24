module Mainline.Mainline
    ( Outbound
    , ServerState
    ) where

import Network.KRPC (KPacket)
import Network.KRPC.Types (CompactInfo)

data ServerState = Null

type Outbound = [(KPacket, CompactInfo)]

serverHandler :: ServerState -> Either String KPacket -> (Outbound, ServerState)
serverHandler s (Left e) = ([], s)
