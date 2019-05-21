{-# LANGUAGE NamedFieldPuns #-}

module Mainline.Mainline
    ( Model (..)
    , Msg (..)
    , ServerState (..)
    , ServerConfig (..)
    , update
    , parseReceivedBytes
    , servePort
    , logParsingErr
    , logHelper
    , logErr
    , getMTstate
    ) where


import Prelude hiding (init, log, filter)
import Data.ByteString (ByteString, empty)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map             as Map
import Data.Set                      (fromAscList, filter)
import Data.Word                     (Word32)
import Data.BEncode                  (encode, decode)
import Data.Time.Clock.POSIX         (POSIXTime)

import Architecture.TEA              (Config (..), run)
import Architecture.Cmd              (Cmd)
import qualified Architecture.Cmd as Cmd
import qualified Architecture.Sub as Sub
import Architecture.Sub              (Sub, Received (..))
import Network.KRPC                  (KPacket (..), scanner)
import Network.Octets                (Octets (..), fromByteString)
import Mainline.RoutingTable
    ( initRoutingTable
    , RoutingTable
    , uncheckedAdd
    , Node (..)
    , NodeStatus (..)
    , exists
    , willAdd
    , nclosest
    )
import Network.KRPC.Types
    ( Port
    , NodeID
    , CompactInfo (..)
    , Message     (..)
    , QueryDat    (..)
    , ResponseDat (..)
    , NodeInfo    (..)
    )

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
 -  - Create procedure to potentially add unknown node ✓
 -  - Extend Sub[.udp] to provide current time ✓
 -  - Test socket recvFrom timeout, catch the event to have more consistent timer
 -  - Timer subscription for cleaning up (transactions, questionable nodes, etc)
 -
 - MILESTONE:
 -
 -   - Repeatedly issue find_nodes to closer and closer nodes, until completion ✓
 -}


{- Constants -}

servePort :: Port
servePort = 51416

seedNodePort :: Port
seedNodePort = 6881
--seedNodePort = 51413

seedNodeHost :: Word32
seedNodeHost = fromOctets [ 82, 221, 103, 244 ]
--seedNodeHost = fromOctets [ 192, 168, 4, 2 ]
--seedNodeHost = fromOctets [ 67, 215, 246, 10 ]

seedNodeInfo :: CompactInfo
seedNodeInfo = CompactInfo seedNodeHost seedNodePort

tidsize :: Int
tidsize = 4

tokensize :: Int
tokensize = 8


{- Data Structures -}

data Model
    = Uninitialized
    | Uninitialized1 ServerConfig ByteString
    | Ready ServerState

data Msg
    = NewNodeId Int ByteString
    | Inbound POSIXTime CompactInfo KPacket
    | ErrorParsing CompactInfo ByteString String
    | SendFirstMessage
        { idx           :: Int
        , sendRecipient :: CompactInfo
        , body          :: Message
        , newtid        :: ByteString
        }
    | SendMessage
        { idx           :: Int
        , sendAction    :: Action
        , targetNode    :: NodeInfo
        , body          :: Message
        , newtid        :: ByteString
        , when          :: POSIXTime
        }
    | SendResponse
        { idx           :: Int
        , targetNode    :: NodeInfo
        , body          :: Message
        , tid           :: ByteString
        }

data ServerState = ServerState
    { transactions :: Transactions
    , conf         :: ServerConfig
    , routingTable :: RoutingTable
    }

data ServerConfig = ServerConfig
    { index      :: Int
    , listenPort :: Port
    , seedNode   :: CompactInfo
    , ourId      :: NodeID
    }

data TransactionState = TransactionState
    {  timeSent  :: POSIXTime
    ,  action    :: Action
    ,  recipient :: NodeInfo
    }

type Transactions = Map.Map NodeID (Map.Map ByteString TransactionState)

data Action = Warmup deriving Eq


main :: IO ()
main = run config

config :: Config Model Msg
config = Config init update subscriptions

init :: (Model, Cmd Msg)
init = (Uninitialized, Cmd.randomBytes 20 (NewNodeId 0))


update :: Msg -> Model -> (Model, Cmd Msg)
-- Error Parsing
update (ErrorParsing ci bs err) model = (model, logParsingErr ci bs err)

-- Get new node id. Init server state, request tid for pinging seed node
update (NewNodeId ix bs) Uninitialized = (initState, initialCmds)
    where
        initialCmds = Cmd.batch [ logmsg, pingSeed ]
        initState = Uninitialized1 conf empty

        conf = ServerConfig
            { index = ix
            , listenPort = servePort
            , seedNode = seedNodeInfo
            , ourId = ourid
            }

        pingSeed = Cmd.randomBytes
            tidsize
            (\t -> SendFirstMessage
                { idx           = ix
                , sendRecipient = seedNodeInfo
                , body          = (Query ourid Ping)
                , newtid        = t
                }
            )

        ourid = (fromByteString bs)

        logmsg = Cmd.log Cmd.DEBUG
            [ "Initializing with node id:"
            , show ourid
            ]


update
    SendFirstMessage
        { sendRecipient
        , body
        , newtid
        }
    (Uninitialized1 conf _) =
        ( Uninitialized1 conf newtid
        , Cmd.batch [ logmsg, sendCmd ]
        )

        where
            logmsg = Cmd.log Cmd.DEBUG
                [ "Sending initial", show kpacket , "to", show sendRecipient ]

            sendCmd =
                Cmd.sendUDP
                    (listenPort conf)
                    sendRecipient
                    (BL.toStrict $ encode kpacket)

            kpacket = KPacket newtid body Nothing


-- Get tid for outound Message
update
    SendMessage
        { sendAction
        , targetNode
        , body
        , newtid
        , when
        }
    (Ready (ServerState { transactions, conf, routingTable })) =
        ( Ready ServerState
            { transactions = trsns
            , conf
            , routingTable
            }
        , Cmd.batch [ logmsg, sendCmd ]
        )

        where
            trsns =
                Map.insertWith
                    (Map.union)
                    (nodeId targetNode)
                    (Map.singleton newtid tstate)
                    transactions

            tstate = TransactionState
                { timeSent = when
                , action = sendAction
                , recipient = targetNode
                }

            kpacket = KPacket newtid body Nothing

            sendCmd =
                Cmd.sendUDP
                    (listenPort conf)
                    (compactInfo targetNode)
                    (BL.toStrict bvalue)

            bvalue = encode kpacket

            logmsg = Cmd.log Cmd.DEBUG
                [ "Sending", show kpacket
                , show targetNode ]


update
    ( Inbound
        now
        client
        (KPacket { message = Response nodeid Pong })
    )
    (Uninitialized1 conf _) = (model, warmup)
        where
            model = Ready ServerState
                { transactions = Map.empty
                , conf = conf
                , routingTable = initRoutingTable ourid
                }

            warmup = Cmd.randomBytes
                tidsize
                (\newid -> SendMessage
                    { idx        = index conf
                    , sendAction = Warmup
                    , targetNode = (NodeInfo nodeid client)
                    , body       = (Query ourid (FindNode ourid))
                    , newtid     = newid
                    , when       = now
                    }
                )

            ourid = ourId conf


update (SendResponse { targetNode, body, tid }) (Ready s) =
    ( Ready s
    , Cmd.batch [ logmsg, sendCmd ]
    )

    where
        sendCmd =
            Cmd.sendUDP
                (listenPort $ conf s)
                (compactInfo targetNode)
                (BL.toStrict bvalue)

        kpacket = KPacket tid body Nothing

        bvalue = encode kpacket

        logmsg = Cmd.log Cmd.DEBUG
            [ "Sending", show kpacket
            , "(" ++ show bvalue ++ ") to", show targetNode ]


-- Receive a message
update
    (Inbound now client (KPacket { transactionId, message, version }))
    (Ready ServerState { transactions, conf, routingTable }) =
        ( Ready newState
        , Cmd.batch
            [ Cmd.log
                Cmd.DEBUG
                [ "IN from" , show client
                , " received:" , show kpacket
                ]
            , maybe
                Cmd.none
                (\t -> if (compactInfo $ recipient t) == client then
                        Cmd.none
                    else
                        Cmd.log Cmd.INFO
                        [ "Client mismatch with state. Expected client "
                        , show $ recipient t
                        , " but received message from "
                        , show client
                        ]
                )
                mtState
            , cmd
            ]
        )

        where
            mtState =
                nodeid >>= \nid -> getMTstate nid transactions transactionId

            nodeid = case message of
                (Query nid _) -> Just nid
                (Response nid _) -> Just nid
                (Error _ _) -> Nothing

            kpacket = KPacket { transactionId, message, version }

            state1 = ServerState
                { transactions
                , conf
                , routingTable
                }

            mkstate2 = \nid -> ServerState
                { transactions = (
                    Map.update
                        ( \trsns ->
                            let
                                ts = Map.delete transactionId trsns
                            in
                                if Map.null ts then Nothing else Just ts
                        )
                        nid
                        transactions
                    )
                , conf
                , routingTable
                }

            (newState, cmd) = case message of
                (Query nid qdat) -> case mtState of
                    Nothing ->
                        respond
                            (NodeInfo nid client)
                            transactionId
                            now
                            qdat
                            state1
                    (Just _) -> logHelper
                        (NodeInfo nid client)
                        (Query nid qdat)
                        "Ignoring a received Query instead of a response from"
                        (mkstate2 nid)
                (Response nid rdat) -> case mtState of
                    (Just tranState) ->
                        handleResponse
                            (NodeInfo nid client)
                            tranState
                            now
                            rdat
                            (mkstate2 nid)
                    Nothing -> logHelper
                        (NodeInfo nid client)
                        (Response nid rdat)
                        "Ignoring a response that wasn't in our transaction state from"
                        (mkstate2 nid)
                e ->
                    logErr client e state1


subscriptions :: Model -> Sub Msg
subscriptions Uninitialized = Sub.none
subscriptions (Uninitialized1 (ServerConfig { listenPort }) _) =
    Sub.udp listenPort parseReceivedBytes
subscriptions (Ready (ServerState { conf = ServerConfig { listenPort } })) =
    Sub.udp listenPort parseReceivedBytes


respond
    :: NodeInfo
    -> ByteString
    -> POSIXTime
    -> QueryDat
    -> ServerState
    -> (ServerState, Cmd Msg)
-- Respond to Ping
respond
    node
    tid
    now
    Ping
    state
    | exists (routingTable state) node = (state, cmd)
    | willAdd (routingTable state) node =
        (newstate, Cmd.batch [cmd, cmds])
    | otherwise = (state, cmd)
        where
            pong =
                Cmd.sendUDP
                    (listenPort (conf state))
                    (compactInfo node)
                    (BL.toStrict $ encode kpacket)

            cmd = Cmd.batch [log, pong]

            kpacket = KPacket tid (Response ourid Pong) Nothing

            ourid = (ourId (conf state))

            (newstate, cmds) = considerNode now state node

            log = Cmd.log Cmd.DEBUG
                [ "sending", show kpacket
                , "to", show node]


-- Respond to FindNode query
respond
    node
    tid
    now
    (FindNode nodeid)
    state
        | exists (routingTable state) node = (state, cmd)
        | willAdd (routingTable state) node = (newstate, Cmd.batch [cmd, cmds])
        | otherwise = (state, cmd)
        where
            nodes = Cmd.sendUDP
                    (listenPort $ conf state)
                    (compactInfo node)
                    (BL.toStrict (encode kpacket))

            cmd = Cmd.batch [logmsg, nodes]

            kpacket = KPacket tid response Nothing

            logmsg = Cmd.log Cmd.DEBUG
                [ "Sending", show kpacket
                , "to", show node ]

            response = Response ourid (Nodes closest)

            ourid = ourId $ conf state

            closest = nclosest nodeid 8 (routingTable state)

            (newstate, cmds) = considerNode now state node

-- Respond to GetPeers query
respond
    node
    tid
    now
    (GetPeers infohash)
    state
        | exists (routingTable state) node = (state, cmd)
        | willAdd (routingTable state) node = (newstate, Cmd.batch [cmd, cmds])
        | otherwise = (state, cmd)
        where
            cmd = Cmd.randomBytes
                tokensize
                (\t -> SendResponse
                    (index $ conf state)
                    node
                    (Response ourid (NodesFound t closest))
                    tid
                )

            ourid = ourId $ conf state

            closest = nclosest infohash 8 (routingTable state)

            (newstate, cmds) = considerNode now state node


-- Respond to AnnouncePeer query
respond
    node
    tid
    _
    (AnnouncePeer _ info _ _)
    state = (state, Cmd.batch [logmsg, sendCmd])
        where
            sendCmd =
                Cmd.sendUDP
                    (listenPort $ conf state)
                    (compactInfo node)
                    (BL.toStrict bvalue)

            kpacket = KPacket tid (Response ourid Pong) Nothing

            ourid = ourId $ conf state

            bvalue = encode kpacket

            logmsg = Cmd.log Cmd.INFO
                [ "Got Peer announcement ", "info_hash:", show info
                , "replying with Pong to", show node
                ]


handleResponse
    :: NodeInfo
    -> TransactionState
    -> POSIXTime
    -> ResponseDat
    -> ServerState
    -> (ServerState, Cmd Msg)
-- Respond to FindNode Response during Warmup
handleResponse
    node
    (TransactionState _ Warmup _)
    now
    (Nodes ninfos)
    state
    | exists (routingTable state) node = (state, Cmd.none)
    | willAdd initialRt node = (newstate, cmds)
    | otherwise = (state, Cmd.none)
        where
            logmsg = Cmd.log Cmd.INFO [ "Adding to routing table:", show node ]
            rt = uncheckedAdd
                    initialRt
                    (Node now node Normal)

            initialRt = routingTable state

            cmds = Cmd.batch $ [ logmsg ] ++ sendCmds

            (newstate, sendCmds) =
                foldl
                    ( \(rt_, l) nodeinfo ->
                        (\(rt__, cmd) -> (rt__, l ++ [cmd]))
                        (considerNode now rt_ nodeinfo)
                    )
                    (state { routingTable = rt }, [])
                    (filter filterNodes (fromAscList ninfos))

-- Ignore the rest for now
handleResponse
    node
    _
    _
    rdat
    state = (state, log)
        where
            log = Cmd.log Cmd.INFO
                [ "Ignoring unsolicited response from"
                , show node, show rdat
                ]


logHelper :: NodeInfo -> Message -> String -> a -> (a, Cmd Msg)
logHelper sender msg logstr state  = (state, log)
            where
                log = Cmd.log Cmd.INFO
                    [ logstr
                    , show sender
                    , show msg
                    ]


logErr :: CompactInfo -> Message -> a -> (a, Cmd Msg)
logErr
    sender
    (Error { errCode, errMsg })
    state = (state, log)
        where
            log = Cmd.log Cmd.INFO
                [ "Received an Error from"
                , show sender
                , (show errCode) ++ ": " ++ show errMsg
                ]

logErr _ _ state = (state, Cmd.none)

logParsingErr :: CompactInfo -> ByteString -> String -> Cmd Msg
logParsingErr ci bs err =
    Cmd.log Cmd.INFO $
        [ "Could not parse received message. Sender:" , show ci
        , "in:" , show bs
        ]
        ++ scnr ++
        [ "reason:", err
        ]

    where
        scnr = either
            (\_ -> [])
            (\bval -> ["scanner:", show $ scanner bval])
            (decode bs)


filterNodes ::  NodeInfo -> Bool
filterNodes node
    =  (1 <= p)
    && (p <= 65535)

    where
        p = (port $ compactInfo node)


-- Node has not yet been contacted
considerNode
    :: POSIXTime
    -> ServerState
    -> NodeInfo
    -> (ServerState, Cmd Msg)
considerNode now state node
    | exists rt node = (state, Cmd.none)
    | noOngoing && willAdd rt node = (state, pingThem)
    | otherwise = (state, Cmd.none)
        where
            noOngoing = (maybe True
                (Map.null . Map.filter filterfunk)
                (Map.lookup (nodeId node) (transactions state)))

            filterfunk = (== Warmup) . action

            rt = routingTable state

            pingThem = Cmd.randomBytes
                tidsize
                (\newid -> SendMessage
                    { idx        = index $ conf state
                    , sendAction = Warmup
                    , targetNode = node
                    , body       = (Query ourid (FindNode ourid))
                    , newtid     = newid
                    , when       = now
                    }
                )

            ourid = ourId $ conf state
    -- node exists in rt => (Model, Cmd.none)
    -- node can be added (bucket not full or ourid in bucket) => send message
    -- if nodes where lastMsgTime < (now - 15m) || status == BeingChecked
    --      then send command or modify TransactionState


parseReceivedBytes :: CompactInfo -> Received -> Msg
parseReceivedBytes compactinfo (Received { bytes, time }) =
    case decode bytes of
        Right kpacket -> Inbound time compactinfo kpacket
        Left msg -> ErrorParsing compactinfo bytes msg


getMTstate :: NodeID -> Transactions -> ByteString -> Maybe TransactionState
getMTstate nid trns tid = Map.lookup nid trns >>= Map.lookup tid
