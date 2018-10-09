module Mainline.Mainline
    ( Model (..)
    , ServerConfig (..)
    , Action (..)
    , TransactionState (..)
    , NotImplemented (..)
    ) where

import qualified Data.Map as Map
import Data.ByteString    (ByteString)

import Network.KRPC.Types
    ( CompactInfo
    , NodeID
    , Port
    )
import Mainline.Bucket    (RoutingTable)
import Network.KRPC.WordInstances ()

data NotImplemented = NotImplemented

data Action
    = PingSeed
    | Warmup

data TransactionState = TransactionState
    {  timeSent    :: NotImplemented
    ,  action      :: Action
    ,  recipient   :: CompactInfo
    }

data ServerConfig = ServerConfig
    { listenPort       :: Port
    , seedNode         :: CompactInfo
    , nodeid           :: NodeID -- Our node id
    }

data Model
    = Uninitialized
    | ServerState
        { transactionState :: Map.Map ByteString TransactionState
        , conf             :: ServerConfig
        , routingTable     :: RoutingTable NodeID
        }
