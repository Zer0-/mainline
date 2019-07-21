{-# LANGUAGE NamedFieldPuns #-}

module Mainline.RoutingTable
    ( Node (..)
    , RoutingTable (..)
    , initRoutingTable
    , uncheckedAdd
    , exists
    , willAdd
    , nclosest
    , changeNode
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sortBy)
import Data.Bits (xor)
import Data.Function (on)
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


data Node = Node
    { lastMsgTime       :: POSIXTime
    , info              :: NodeInfo
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
willAdd rt nodeinfo = nid /= (getOwnId rt) && willInsert nid (bucket rt)
    where
        nid = (nodeId nodeinfo)


exists :: RoutingTable -> NodeInfo -> Bool
exists rt nodeinfo = Map.member (nodeId nodeinfo) (nodes rt)


initRoutingTable :: NodeID -> RoutingTable
initRoutingTable nodeid = RoutingTable bucket Map.empty
    where
        bucket = Bucket nodeid bucketsize 0 (((^) :: Integer -> Integer -> Integer) 2 160) Set.empty


getOwnId :: RoutingTable -> NodeID
getOwnId = getId . bucket


nclosest :: NodeID -> Int -> RoutingTable -> [NodeInfo]
nclosest nid n rt = map info $ take n $ sortBy f $ Map.elems $ nodes rt
    where
        f = g `on` getid

        g i j
            | i == j                    = EQ
            | nid `xor` i < nid `xor` j = LT
            | otherwise                 = GT


        getid = (nodeId . info)
