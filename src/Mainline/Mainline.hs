{-# LANGUAGE NamedFieldPuns #-}

module Mainline.Mainline
    ( Model (..)
    , Msg (..)
    , ServerState (..)
    , ServerConfig (..)
    , Action (..)
    , Cmd
    , update
    , parseReceivedBytes
    , onParsingErr
    , logHelper
    , logErr
    , getMTstate
    , tidsize
    ) where


import Prelude hiding (init, log, filter)
import qualified Data.List as L
import Data.ByteString (ByteString, empty)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map             as Map
import Data.Map                       (Map)
import qualified Data.Set             as Set
import Data.Set                      (fromAscList, filter, Set)
import Data.BEncode                  (encode, decode)
import Data.Time.Clock.POSIX         (POSIXTime)
import Data.Array                    (Array, (!), indices, listArray)

--import Architecture.TEA              (Config (..), run)
--import Architecture.Cmd              (Cmd)
import qualified Architecture.Cmd as Cmd
--import qualified Architecture.Sub as Sub
--import Architecture.Sub              (Sub, Received (..))
import Architecture.Sub              (Received (..))
import Network.KRPC                  (KPacket (..), scanner)
import Network.KRPC.Helpers          (stringpack)
import Network.Octets (fromByteString)
import Mainline.RoutingTable
    ( initRoutingTable
    , RoutingTable
    , uncheckedAdd
    , Node (..)
    , exists
    , willAdd
    , nclosest
    , questionable
    , remove
    , updateTime
    , orderingf
    )
import Network.KRPC.Types
    ( Port
    , NodeID
    , CompactInfo (..)
    , Message     (..)
    , QueryDat    (..)
    , ResponseDat (..)
    , NodeInfo    (..)
    , InfoHash
    )
import Mainline.SQL (Schemas);

type Cmd msg = Cmd.Cmd msg Schemas

{- Constants -}

tidsize :: Int
tidsize = 4

tokensize :: Int
tokensize = 8

responseTimeout :: Int
responseTimeout = 120

{- Data Structures -}

data Model
    = Uninitialized ServerConfig
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
    | TimeoutTransactions POSIXTime
    | MaintainPeers POSIXTime
    | Reset
    | PeersFoundResult POSIXTime Int NodeID InfoHash [CompactInfo]
    | NodeAdded CompactInfo
    | UDPError

data ServerState = ServerState
    { transactions :: Transactions
    , conf         :: ServerConfig
    , routingTable :: RoutingTable
    , gettingPeers :: Map InfoHash (Set NodeID)
    }

data ServerConfig = ServerConfig
    { index      :: Int
    , listenPort :: Port
    , ourId      :: NodeID
    , seedNodes  :: Array Int CompactInfo
    , bucketSize :: Int
    }

data TransactionState = TransactionState
    {  timeSent  :: POSIXTime
    ,  action    :: Action
    ,  recipient :: NodeInfo
    }

type Transactions = Map NodeID (Map ByteString TransactionState)

data Action
    = Warmup
    | KeepAlive
    | NewPeersSearch InfoHash
    | GettingPeers InfoHash
    deriving Eq


{-
main :: IO ()
main = run config

config :: Config Model Msg
config = Config init update subscriptions

init :: (Model, Cmd Msg)
init = (Uninitialized 0, Cmd.randomBytes 20 NewNodeId)
-}


update :: Msg -> Model -> (Model, Cmd Msg)
-- Error Parsing
update (ErrorParsing ci bs err) m =
    case m of
        Uninitialized _ -> (m, logParsingErr ci bs err)
        (Uninitialized1 c _) -> (m, onParsingErr (listenPort c) ci bs err)
        Ready state -> (m, onParsingErr (listenPort $ conf state) ci bs err)

-- Get new node id. Init server state, request tid for pinging seed node
update (NewNodeId _ bs) (Uninitialized cfg) =
    (Uninitialized cfg { ourId = ourid }, initialCmds)
    where
        initialCmds = Cmd.batch [ logmsg, pingSeed ]

        seedsIx = fromInteger $ ((fromByteString bs) :: Integer)
            `mod` fromIntegral (length $ indices (seedNodes cfg))

        pingSeed = Cmd.randomBytes
            tidsize
            (\t -> SendFirstMessage
                { idx           = index cfg
                , sendRecipient = (seedNodes cfg) ! seedsIx
                , body          = (Query ourid Ping)
                , newtid        = t
                }
            )

        ourid = (fromByteString bs)

        logmsg = Cmd.log Cmd.INFO
            [ "Initializing with node id:"
            , show ourid
            ]


update
    SendFirstMessage
        { sendRecipient
        , body
        , newtid
        }
    (Uninitialized cfg) =
        ( Uninitialized1 cfg newtid
        , Cmd.batch [ logmsg, sendCmd ]
        )

        where
            logmsg = Cmd.log Cmd.DEBUG
                [ "Sending initial", show kpacket , "to", show sendRecipient ]

            sendCmd =
                Cmd.sendUDP
                    (listenPort cfg)
                    sendRecipient
                    (BL.toStrict $ encode kpacket)
                    UDPError

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
    (Ready (ServerState { transactions, conf, routingTable, gettingPeers })) =
        ( Ready ServerState
            { transactions = trsns
            , conf
            , routingTable
            , gettingPeers
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
                    UDPError

            bvalue = encode kpacket

            logmsg = Cmd.log Cmd.DEBUG
                [ "Sending", show kpacket
                , show targetNode ]

-- Pong response from bootstrap node
update
    ( Inbound
        now
        client
        (KPacket { message = Response nodeid Pong })
    )
    (Uninitialized1 conf _) = (model, Cmd.batch [logmsg, warmup])
        where
            model = Ready ServerState
                { transactions = Map.empty
                , conf         = conf
                , routingTable = initRoutingTable (bucketSize conf) ourid
                , gettingPeers = Map.empty
                }

            warmup = Cmd.randomBytes
                tidsize
                (\newid -> SendMessage
                    { idx        = index conf
                    , sendAction = Warmup
                    , targetNode = NodeInfo nodeid client
                    , body       = Query ourid (FindNode ourid)
                    , newtid     = newid
                    , when       = now
                    }
                )

            logmsg = Cmd.log Cmd.INFO ["Initial Pong from", show client]

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
                UDPError

        kpacket = KPacket tid body Nothing

        bvalue = encode kpacket

        logmsg = Cmd.log Cmd.DEBUG
            [ "Sending", show kpacket
            , "(" ++ show bvalue ++ ") to", show targetNode ]


-- Receive a message
update
    (Inbound now client (KPacket { transactionId, message, version }))
    (Ready
        ServerState
            { transactions
            , conf
            , routingTable = rt
            , gettingPeers
            }) =
        ( Ready state3
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
                        Cmd.log Cmd.DEBUG
                        [ "Client mismatch with state. Expected client "
                        , show $ recipient t
                        , " but received message from "
                        , show client
                        ]
                )
                mTState
            , cmd
            ]
        )

        where
            mTState =
                nodeid >>= \nid -> getMTstate nid transactions transactionId

            nodeid = case message of
                (Query nid _) -> Just nid
                (Response nid _) -> Just nid
                (Error _ _) -> Nothing

            kpacket = KPacket { transactionId, message, version }

            state1 = ServerState
                { transactions
                , conf
                , routingTable = rt
                , gettingPeers
                }

            withoutTransaction = \nid -> ServerState
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
                , routingTable = rt
                , gettingPeers
                }

            (state2, cmd) = case message of
                (Query nid qdat) ->
                    respond
                        (NodeInfo nid client)
                        transactionId
                        now
                        qdat
                        state1
                (Response nid rdat) -> case mTState of
                    (Just tranState) ->
                        handleResponse
                            (NodeInfo nid client)
                            tranState
                            now
                            rdat
                            (withoutTransaction nid)
                    Nothing -> logHelper
                        (NodeInfo nid client)
                        (Response nid rdat)
                        "Ignoring a response that wasn't in our transaction state from"
                        (withoutTransaction nid)
                e ->
                    logErr client e state1

            state3 = case nodeid of
                (Just nid) ->
                    state2
                        { routingTable =
                            updateTime (routingTable state2) nid now
                        }
                Nothing -> state2

update (Inbound _ ci kpacket) (Uninitialized cfg) = ((Uninitialized cfg), log)
    where
        log = Cmd.log Cmd.DEBUG
            [ "Ignoring received message while Uninitialized"
            , show ci
            , show kpacket
            ]

update (Inbound _ ci kpacket) (Uninitialized1 cfg tid) =
    ((Uninitialized1 cfg tid), log)

    where
        log = Cmd.log Cmd.DEBUG
            [ "Ignoring received message while Uninitialized1"
            , show ci
            , show kpacket
            ]

update (TimeoutTransactions _) (Uninitialized1 cfg _) =
    ( Uninitialized cfg
    , Cmd.randomBytes 20 $ NewNodeId (index cfg)
    )

update (TimeoutTransactions now) (Ready state) =
    (Ready state { transactions = newtrns, routingTable = newrt }, log)

    where
        want :: [[(NodeID, ByteString, TransactionState)]]
        want = map
            (\(ci, m) -> [(ci, bs, tstate) | (bs, tstate) <- Map.assocs m])
            (Map.assocs trns)

        removes :: [[(NodeID, ByteString, TransactionState)]]
        removes = L.filter (not . null) $ map (L.filter ff) want

        ff (_, _, tstate) =
            now - (timeSent tstate) >= fromIntegral responseTimeout

        trns = transactions state

        newtrns :: Transactions
        newtrns = L.foldl' f trns removes

        f :: Transactions -> [(NodeID, ByteString, a)] -> Transactions
        f t [] = t
        f t ts = Map.update (g ts) (myfst $ head ts) t

        g :: [(NodeID, ByteString, a)]
          -> Map ByteString TransactionState
          -> Maybe (Map ByteString TransactionState)
        g ts m
            | length ts == Map.size m = Nothing
            | otherwise = Just $ Map.withoutKeys m (Set.fromList (map mysnd ts))

        myfst :: (a, b, c) -> a
        myfst (x, _, _) = x

        mysnd :: (a, b, c) -> b
        mysnd (_, x, _) = x

        mythrd :: (a, b, c) -> c
        mythrd (_, _, x) = x

        newrt :: RoutingTable
        newrt = L.foldl' rm (routingTable state) removes

        rm :: RoutingTable
           -> [(NodeID, ByteString, TransactionState)]
           -> RoutingTable
        rm rt ts = L.foldl' onRemove rt (map mythrd ts)

        onRemove :: RoutingTable -> TransactionState -> RoutingTable
        onRemove rt (TransactionState { action = KeepAlive, recipient }) =
            remove rt (nodeId recipient)
        onRemove rt _ = rt

        log = Cmd.log Cmd.DEBUG [ "removed",
            show (sum $ map length removes), "timed out transactions" ]


update (MaintainPeers now) (Ready state) =
    (Ready state, Cmd.batch $ map prepareQuery q)

    where
        q :: [ Node ]
        q = questionable (routingTable state) now

        prepareQuery :: Node -> Cmd Msg
        prepareQuery node = Cmd.randomBytes
            tidsize
            (\newid -> SendMessage
                { idx        = index $ conf state
                , sendAction = KeepAlive
                , targetNode = (info node)
                , body       = (Query ourid Ping)
                , newtid     = newid
                , when       = now
                }
            )

        ourid = ourId $ conf $ state

update Reset (Ready state) =
    ( Uninitialized cfg
    , Cmd.randomBytes 20 $ NewNodeId (index cfg)
    )

    where
        cfg1 = conf state
        cfg = cfg1 { seedNodes = listArray (0, 19) newseeds }

        newseeds = map compactInfo $
            nclosest (ourId cfg1) 20 (routingTable state) -- "random" subset

update (TimeoutTransactions _) m = (m, Cmd.none)
update (MaintainPeers _) m = (m, Cmd.none)
update Reset m = (m, Cmd.none)
update (NodeAdded _) m = (m, Cmd.none)


-- Explicitly list undefined states
update (NewNodeId _ _)       (Uninitialized1 _ _)  = undefined
update (NewNodeId _ _)       (Ready _)             = undefined
update (SendFirstMessage {}) (Uninitialized1 _ _ ) = undefined
update (SendFirstMessage {}) (Ready _)             = undefined
update (SendMessage {})      (Uninitialized _)     = undefined
update (SendMessage {})      (Uninitialized1 _ _)  = undefined
update (SendResponse {})     (Uninitialized _)     = undefined
update (SendResponse {})     (Uninitialized1 _ _)  = undefined
update (PeersFoundResult _ _ _ _ _) _              = undefined
update UDPError              _                     = undefined

{-
subscriptions :: Model -> Sub Msg
subscriptions Uninitialized = Sub.none
subscriptions (Uninitialized1 (ServerConfig { listenPort }) _) =
    Sub.udp listenPort parseReceivedBytes
subscriptions (Ready (ServerState { conf = ServerConfig { listenPort } })) =
    Sub.udp listenPort parseReceivedBytes
-}

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
                    UDPError

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
                    UDPError

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
    (AnnouncePeer _ info _ _ mname)
    state = (state, Cmd.batch [logmsg, sendCmd])
        where
            sendCmd =
                Cmd.sendUDP
                    (listenPort $ conf state)
                    (compactInfo node)
                    (BL.toStrict bvalue)
                    UDPError

            kpacket = KPacket tid (Response ourid Pong) Nothing

            ourid = ourId $ conf state

            bvalue = encode kpacket

            logmsg = Cmd.log Cmd.DEBUG
                [ "Got Peer announcement ", "info_hash:", show info
                , maybe "" (((++) "name: ") . show) mname
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
            logmsg = Cmd.log Cmd.DEBUG [ "Adding to routing table:", show node ]
            rt = uncheckedAdd initialRt (Node now node)

            initialRt = routingTable state

            cmds = Cmd.batch $ logmsg : bounce : sendCmds

            (newstate, sendCmds) =
                foldl
                    ( \(s, l) nodeinfo ->
                        (\(s_, cmd) -> (s_, l ++ [cmd]))
                        (considerNode now s nodeinfo)
                    )
                    (state { routingTable = rt }, [])
                    (filter filterNodes (fromAscList ninfos))

            bounce = Cmd.bounce $ NodeAdded (compactInfo node)


-- Respond first GetPeers Nodes Response
handleResponse
    respondingNode
    (TransactionState _ (NewPeersSearch infohash) _)
    now
    response
    state =
        handleResponse
            respondingNode
            (TransactionState undefined (GettingPeers infohash) undefined)
            now
            response
            state { gettingPeers = newGettingPeers }

        where
            ongoing = gettingPeers state

            newGettingPeers =
                if Map.member infohash ongoing then ongoing
                else Map.insert infohash (Set.empty) (gettingPeers state)

-- Respond to GetPeers Nodes Response when looking for peers
handleResponse
    respondingNode
    (TransactionState _ (GettingPeers infohash) _)
    now
    (NodesFound _ nodes)
    state
        | Map.member infohash ongoing =
            ( state { gettingPeers = newOngoing }
            , Cmd.batch $ map mksend closer
            )
        | otherwise = (state, Cmd.none)
        where
            closer = L.filter
                ( \n ->
                    ( (== GT)
                    . (orderingf infohash (nodeId respondingNode))
                    . nodeId
                    ) n
                    && filterNodes n
                    && Set.notMember (nodeId n) contacted
                )
                nodes

            ongoing = gettingPeers state

            newOngoing = Map.insert infohash newContacted ongoing

            contacted = ongoing Map.! infohash

            newContacted =
                contacted `Set.union` Set.fromList (map nodeId closer)

            mksend node = Cmd.randomBytes tidsize
                (\newid -> SendMessage
                    { idx = index config
                    , sendAction = GettingPeers infohash
                    , targetNode = node
                    , body       = Query (ourId config) (GetPeers infohash)
                    , newtid     = newid
                    , when       = now
                    }
                )

            config = conf state

handleResponse
    _
    (TransactionState _ (GettingPeers infohash) _)
    now
    (PeersFound _ peers)
    state
        | Map.member infohash (gettingPeers state) =
            (state, Cmd.bounce (PeersFoundResult now idx ourid infohash peers))
        | otherwise = (state, Cmd.none)

        where
            cf = conf state
            ourid = ourId $ cf
            idx = index cf

handleResponse
    _
    (TransactionState _ KeepAlive _)
    _
    Pong
    state = (state, Cmd.none) -- updateTime is called anyway

-- Ignore the rest
handleResponse
    node
    _
    _
    rdat
    state = (state, log)
        where
            log = Cmd.log Cmd.DEBUG
                [ "Ignoring unsolicited response from"
                , show node, show rdat
                ]


logHelper :: NodeInfo -> Message -> String -> a -> (a, Cmd Msg)
logHelper sender msg logstr state  = (state, log)
            where
                log = Cmd.log Cmd.DEBUG
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
            log = Cmd.log Cmd.DEBUG
                [ "Received an Error from"
                , show sender
                , (show errCode) ++ ": " ++ show errMsg
                ]

logErr _ _ state = (state, Cmd.none)

onParsingErr :: Port -> CompactInfo -> ByteString -> String -> Cmd Msg
onParsingErr p ci bs err = Cmd.batch [ logParsingErr ci bs err,  logmsg, reply ]
    where
        reply = Cmd.sendUDP p ci (BL.toStrict $ encode kpacket) UDPError
        kpacket = KPacket empty (Error 203 (stringpack msg)) Nothing
        msg = "Could not parse received message: " ++ err
        logmsg = Cmd.log Cmd.DEBUG [ "Sending", show kpacket , "to", show ci ]


logParsingErr :: CompactInfo -> ByteString -> String -> Cmd Msg
logParsingErr ci bs err =
    Cmd.log Cmd.DEBUG $
        [ "Could not parse received message. Sender:" , show ci
        , "in:" , show bs
        ]
        ++ scnr ++
        [ "reason:", err
        ]

    where
        scnr = either
            (const [])
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
    | noOngoing && willAdd rt node = (state, prepareQuery)
    | otherwise = (state, Cmd.none)
        where
            noOngoing = (maybe True
                (Map.null . Map.filter filterfunk)
                (Map.lookup (nodeId node) (transactions state)))

            filterfunk = (== Warmup) . action

            rt = routingTable state

            prepareQuery = Cmd.randomBytes
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


parseReceivedBytes :: CompactInfo -> Received -> Msg
parseReceivedBytes compactinfo (Received { bytes, time }) =
    case decode bytes of
        Right kpacket -> Inbound time compactinfo kpacket
        Left msg -> ErrorParsing compactinfo bytes msg


getMTstate :: NodeID -> Transactions -> ByteString -> Maybe TransactionState
getMTstate nid trns tid = Map.lookup nid trns >>= Map.lookup tid
