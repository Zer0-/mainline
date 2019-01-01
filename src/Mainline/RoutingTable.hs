module Mainline.RoutingTable
    ( Node (..)
    , RoutingTable
    , initRoutingTable
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Word (Word32)
import Data.Digest.SHA1 (Word160 (..))
import Data.Time.Clock.POSIX (POSIXTime)

import Mainline.Bucket (Bucket (Bucket))
import Network.KRPC.Types (NodeID, CompactInfo)
--
--Maximum size of a bucket before it must be split
bucketsize :: Int
bucketsize = 8


data Node = Node
    { timeLastMessage   :: POSIXTime
    , info              :: CompactInfo
    }


data RoutingTable = RoutingTable
    { bucket :: Bucket NodeID
    , nodes :: Map.Map NodeID Node
    }


initRoutingTable :: NodeID -> RoutingTable
initRoutingTable nodeid = RoutingTable bucket Map.empty
    where
        bucket = Bucket nodeid bucketsize minword maxword Set.empty
        minword = Word160 i i i i i
        maxword = Word160 j j j j j
        i = minBound :: Word32
        j = maxBound :: Word32
