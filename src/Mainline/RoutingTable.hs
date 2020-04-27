{-# LANGUAGE NamedFieldPuns #-}

module Mainline.RoutingTable
    ( Node (..)
    , RoutingTable (..)
    , initRoutingTable
    , uncheckedAdd
    , exists
    , willAdd
    , nclosest
    , questionable
    , remove
    , updateTime
    , orderingf
    ) where

import qualified Data.Set as Set
import Data.Bits (xor)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Maybe (isJust)

import qualified Data.BinaryTrie as BPT
import Mainline.Bucket
    ( Bucket (Bucket)
    , insert
    , willInsert
    , getId
    , delete
    )
import Network.KRPC.Types (NodeID, NodeInfo (..))

--Maximum size of a bucket before it must be split
-- bucketsize :: Int
-- bucketsize = 8

minQDurationSeconds :: Int
minQDurationSeconds = 15 * 60

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
    , nodes :: BPT.Trie Node
    }


uncheckedAdd :: RoutingTable -> Node -> RoutingTable
uncheckedAdd (RoutingTable { bucket, nodes }) node =
    RoutingTable
        { bucket = insert nodeid bucket
        , nodes = BPT.insert nodeid node nodes
        }

    where
        nodeid = nodeId $ info node


willAdd :: RoutingTable -> NodeInfo -> Bool
willAdd rt nodeinfo = nid /= (getOwnId rt) && willInsert nid (bucket rt)
    where
        nid = (nodeId nodeinfo)


exists :: RoutingTable -> NodeInfo -> Bool
exists rt nodeinfo = isJust $ BPT.lookup (nodes rt) (nodeId nodeinfo)


initRoutingTable :: Int -> NodeID -> RoutingTable
initRoutingTable bucketsize nodeid = RoutingTable bucket BPT.empty
    where
        bucket = Bucket nodeid bucketsize 0 (((^) :: Integer -> Integer -> Integer) 2 160) Set.empty


getOwnId :: RoutingTable -> NodeID
getOwnId = getId . bucket


nclosest :: NodeID -> Int -> RoutingTable -> [NodeInfo]
nclosest nid n rt = map info $ BPT.nclosest n (nodes rt) nid


orderingf :: Integer -> Integer -> Integer -> Ordering
orderingf target = f
    where
        f i j
            | i == j                          = EQ
            | target `xor` i < target `xor` j = LT
            | otherwise                       = GT


questionable :: RoutingTable -> POSIXTime -> [ Node ]
questionable rt now = filter f $ BPT.elems $ nodes rt
    where
        f :: Node -> Bool
        f n = lastMsgTime n < now - (fromIntegral minQDurationSeconds)


remove :: RoutingTable -> NodeID -> RoutingTable
remove RoutingTable { bucket, nodes } nid =
    RoutingTable (delete nid bucket) (BPT.delete nodes nid)


updateTime :: RoutingTable -> NodeID -> POSIXTime -> RoutingTable
updateTime rt nid now = rt
    { nodes =
        BPT.modify
            (\n -> Just $ n { lastMsgTime = now })
            (nodes rt)
            nid
    }
