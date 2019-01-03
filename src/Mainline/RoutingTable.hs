{-# LANGUAGE NamedFieldPuns #-}

module Mainline.RoutingTable
    ( Node (..)
    , RoutingTable
    , initRoutingTable
    , uncheckedAdd
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Word (Word32)
import Data.Digest.SHA1 (Word160 (..))
import Data.Time.Clock.POSIX (POSIXTime)
import Network.KRPC.WordInstances()

import Mainline.Bucket (Bucket (Bucket), insert)
import Network.KRPC.Types (NodeID, NodeInfo (..))

--Maximum size of a bucket before it must be split
bucketsize :: Int
bucketsize = 8


data Node = Node
    { lastMsgTime       :: POSIXTime
    , info              :: NodeInfo
    }


data RoutingTable = RoutingTable
    { bucket :: Bucket NodeID
    , nodes :: Map.Map NodeID Node
    }


uncheckedAdd :: RoutingTable -> Node -> RoutingTable
uncheckedAdd (RoutingTable { bucket, nodes }) node =
    RoutingTable
        { bucket = insert (nodeid :: Word160) bucket
        , nodes = Map.insert nodeid node nodes
        }

    where
        nodeid = nodeId $ info node


initRoutingTable :: NodeID -> RoutingTable
initRoutingTable nodeid = RoutingTable bucket Map.empty
    where
        bucket = Bucket nodeid bucketsize minword maxword Set.empty
        minword = Word160 i i i i i
        maxword = Word160 j j j j j
        i = minBound :: Word32
        j = maxBound :: Word32
