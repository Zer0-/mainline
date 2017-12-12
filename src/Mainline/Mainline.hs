module Mainline.Mainline
    ( Outbound
    , ServerState (..)
    , NotImplemented (..)
    , serverHandler
    ) where

import qualified Data.Map as Map
import Data.ByteString    (ByteString)

import Network.KRPC       (KPacket (..))
import Network.KRPC.Types (CompactInfo, Message (..), NodeID, NodeInfo (..))
import Mainline.Bucket    (RoutingTable, willInsert)
import Network.KRPC.WordInstances ()

data NotImplemented = NotImplemented

data Action = InsertNew

data TransactionState = TransactionState
    {  timeSent    :: NotImplemented
    -- action to take
    ,  action      :: Action
    -- who we're sending to
    ,  compactinfo :: CompactInfo
    }

data ServerState = ServerState
    { transactionState :: Map.Map ByteString NotImplemented
    , nodeid           :: NodeID -- Our node id
    , routingTable     :: RoutingTable NodeID
    }

-- probably should be (CompactInfo, (Either KPacket Message, NotImplemented))
-- above comment implies there's a case where we need to send a KPacket
-- and also set state associated with it
-- difference between kpacket and message is that kpacket already has
-- a transaction id, and a message in an outbound will get one on the way out
-- by the app module.
-- if we're sending a response, it's going to have the same tid as the
-- request that came in. The current assumption is that a response doesn't
-- need to be in transactionState
type Outbound = (CompactInfo, Either KPacket (Message, NotImplemented))

-- Do we need the current timestamp here too?
serverHandler :: ServerState -> CompactInfo -> KPacket -> (ServerState, [Outbound])

{-
 - What are we doing here?
 - What about Bucket?
 -
 - What are the two cases a KPacket can be?
 -      1. A Query (Request)
 -      2. A Response
 -      3. Error
 -
 - Query    NodeID { Ping, FindNode, AskPeers, AnnouncePeer }
 - Response NodeID { Ping, Nodes,    PeersFound Token {Values, Nodes} }
 -
 - Let's start by saying what we need to do for each case.
 - Start with case of Query:
 -
 -}

serverHandler state compactinfo (KPacket tid (Query nid Ping)) =
    ifNewNode state nodeinfo [outbound]
        where
            nodeinfo = NodeInfo nid compactinfo
            outbound =
                ( compactinfo
                , Left $ KPacket tid (Response (nodeid state) Ping)
                )

-- Query
-- - This is potentially a new node
-- - Try to add new node to bucket with state set to (we don't even have a Node type fool)
--   questionable
--   Questionable Maybe timestamp - Nothing means we never queried, Just sent_ping_time
-- - in the maintenance step ping all questionable nodes
-- - Query Ping -> Response Ping
--
-- Response
-- - update Node's last seen timestamp, set status to Good
serverHandler state _ _ = (state, [])
{-
 - Before we can implement this we need to check if a nodeid falls in the range
 - of the bucket. Need to check if toInteger Word160 is correct (our NodeIds
 - are stored how? Word32 in platform endian in big endian order.
    -- word160 is a bytestring parsed big-endian, toInteger reads big-endian
 - toInteger assumes things are stored in big endian order, but Word32 -> Integer
 - is done using fromIntegral, so it depends if fromIntegral assumes Word32 is
 - in platform endianness or not.
 -
 - Define insert and isnew separately
 - (gotcha: the spec says to discard new nodes if they don't fit into a bucket
 - full of good nodes. We respond to pings so if a new node keeps pinging us
 - this will cause us to look through the bucket each time
 -    --so what, why even write this?)
 -
 - The spec here is annoying
 -
 - When the bucket is full of good nodes, the new node is simply discarded.
 - If any nodes in the bucket are known to have become bad,
 - then one is replaced by the new node.
 -
 -      -- the new node isn't good. It just sent us a request, never a response. (it's new duh)
 -      - Good Node:
 -                              that
 -                               \/
 -          A good node is a node has responded to one of our queries within
 -              the last 15 minutes.
 -          A node is also good if it has ever responded to one of our queries
 -              and has sent us a query within the last 15 minutes.            <- For fucks sake.
 -                                                                                Is this a typo?
 -                                                                                Did they mean
 -                                                                                "or" instead of
 -                                                                                "and"?
 -
 -          stmt = (A & B) | (A & (Q & B))
 -              ≡ (A & B)
 -
 -           where
 -            A: sent Response
 -            B: t < 15 -- reason i was confused
 -            Q: sent Query
 -
 -           B => (A | Q)
 -
 -          But then we can have a good node that never sent us a Response
 -
 -          revised stmt = (A & B) | (A & (Q & B'))
 -
 -           where
 -            A: sent Response
 -            B:  tr < 15  (time of last response)
 -            B': tq < 15  (time of last query)
 -            Q: sent Query
 -
 -
 - Any reason to not ping first and add a non-good node to the bucket?
 -
 -                                                 that
 -                                                  \/
 - If there are any questionable nodes in the bucket have not been seen
 -  in the last 15 minutes, the least recently seen node is pinged.
 - If the pinged node responds then the next least recently seen
 - questionable node is pinged until one fails to respond or all of the nodes
 -  in the bucket are known to be good.
 - If a node in the bucket fails to respond to a ping, it is suggested to
 -  try once more before discarding the node and replacing it with a new
 -  good node. In this way, the table fills with stable long running nodes.
 -
 -
 -
 - we know it's new so we need to issue a Ping first.
pingNew :: ServerState -> CompactInfo -> NodeId -> (ServerState, [Outbound])
    | -}

ifNewNode :: ServerState -> NodeInfo -> [Outbound] -> (ServerState, [Outbound])
ifNewNode state nodeinfo outbounds
    | (isNodeNew state nodeinfo) && (willInsert (nodeId nodeinfo) (routingTable state)) =
        (state2, outbounds ++ outbounds2)
    | otherwise = (state, outbounds)
            where
                state2 = state --TODO: insert nodeinfo into state so we know
                --how to process on the return trip (needs transaction id
                --and node id to confirm response from same node)
                outbounds2 = []

isNodeNew :: ServerState -> NodeInfo -> Bool
isNodeNew _ _ = True
