module Mainline.Mainline
    ( Model (..)
    , ServerConfig (..)
    , Action (..)
    , TransactionState (..)
    , Transactions
    ) where

import qualified Data.Map as Map
import Data.ByteString    (ByteString)
import Data.Time.Clock.POSIX (POSIXTime)

import Network.KRPC.Types
    ( CompactInfo
    , NodeInfo
    , NodeID
    , Port
    )
import Mainline.RoutingTable (RoutingTable)

data Action = Warmup


{-
 - TODO:
 -
 -  - Create a data type for Node ✓
 -      - t_last_msg ✓
 -      - t_added ?
 -      - CompactInfo ✓
 -
 -  - Store nodes in ServerState in a map of NodeID -> Node ✓
 -  - [i] Combine RoutingTable and nodes map into one structure in own module ✓
 -  - Send out FindNode rather than initial Ping ✓
 -  - Create procedure to add seed node to ServerState (routing table) ✓
 -  - Create procedure to potentially add unknown node (replace bad one etc)
 -  - Extend Sub[.udp] to provide current time ✓
 -  - Test socket recvFrom timeout, catch the event to have more consistent timer
 -  - Timer subscription for cleaning up (transactions, questionable nodes, etc)
 -
 - MILESTONE:
 -
 -   - Repeatedly issue find_nodes to closer and closer nodes, until completion
 -}


data TransactionState = TransactionState
    {  timeSent    :: POSIXTime
    ,  action      :: Action
    ,  recipient   :: NodeInfo
    }

type Transactions = Map.Map NodeID (Map.Map ByteString TransactionState)

data ServerConfig = ServerConfig
    { listenPort       :: Port
    , seedNode         :: CompactInfo
    , ourId            :: NodeID
    }

data Model
    = Uninitialized
    | Uninitialized1 NodeID
    | ServerState
        { transactions     :: Transactions
        , conf             :: ServerConfig
        , routingTable     :: RoutingTable
        }
