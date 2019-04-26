{-# LANGUAGE NamedFieldPuns #-}

module Mainline.RoutingTable
    ( Node (..)
    , NodeStatus (..)
    , RoutingTable
    , initRoutingTable
    , uncheckedAdd
    , exists
    , getOwnId
    , willAdd
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (POSIXTime)

import Mainline.Bucket
    ( Bucket (Bucket)
    , insert
    , willInsert
    , getId
    )
import Network.KRPC.Types (NodeID, NodeInfo (..))

--Maximum size of a bucket before it must be split
bucketsize :: Int
bucketsize = 8


data NodeStatus = Normal | BeingReplaced | BeingChecked
-- if a Node is BeingChecked, we need to find that TransactionState
-- in our transaction state (maybe put the tid into the NodeStatus(s)
-- and replace the information with that of BeingReplaced (as well as
-- the node's status) in the off-chance that the checked node will not
-- respond.
--
-- Add BeingAdded status because for every find_node iteration we can add many
-- nodes, and we don't want to ping those we can't add (because we thought
-- the bucket had room)
--      -decided not to do this


data Node = Node
    { lastMsgTime       :: POSIXTime
    , info              :: NodeInfo
    , status            :: NodeStatus
    }


-- Can multiple nodes have the same CompactInfo?
-- What would be the consequences?
--      If we allowed this,
--      A single node, during the warmup phase, could give us
--      fake nodes pointing to itself that are "artificially"
--      close to our self. This would lead to poor CompactInfo
--      diversity in our routing table.
data RoutingTable = RoutingTable
    { bucket :: Bucket NodeID
    , nodes :: Map.Map NodeID Node
    }


uncheckedAdd :: RoutingTable -> Node -> RoutingTable
uncheckedAdd (RoutingTable { bucket, nodes }) node =
    RoutingTable
        { bucket = insert nodeid bucket
        , nodes = Map.insert nodeid node nodes
        }

    where
        nodeid = nodeId $ info node


changeNode :: (Node -> Node) -> NodeID -> RoutingTable -> RoutingTable
changeNode f k rt = rt { nodes = Map.adjust f k (nodes rt) }


willAdd :: RoutingTable -> NodeInfo -> Bool
willAdd rt nodeinfo = willInsert (nodeId nodeinfo) (bucket rt)


exists :: RoutingTable -> NodeInfo -> Bool
exists rd nodeinfo = Map.member (nodeId nodeinfo) (nodes rd)


initRoutingTable :: NodeID -> RoutingTable
initRoutingTable nodeid = RoutingTable bucket Map.empty
    where
        bucket = Bucket nodeid bucketsize 0 (2^160) Set.empty


getOwnId :: RoutingTable -> NodeID
getOwnId = getId . bucket
