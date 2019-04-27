{-# LANGUAGE NamedFieldPuns #-}

import Prelude hiding (init, log, filter)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map             as Map
import Data.Set                      (fromAscList, filter)
import Data.Word                     (Word32)
import Data.BEncode                  (encode, decode)
import Data.Time.Clock.POSIX         (POSIXTime)

import Architecture.TEA              (Config (..), run)
import qualified Architecture.Cmd as Cmd
import qualified Architecture.Sub as Sub
import Architecture.Sub              (Sub, Received (..))
import Network.KRPC                  (KPacket (..), scanner)
import Network.Octets                (Octets (..), fromByteString)
import Mainline.RoutingTable
    ( RoutingTable
    , initRoutingTable
    , uncheckedAdd
    , Node (..)
    , NodeStatus (..)
    , exists
    , willAdd
    , getOwnId
    )
import Network.KRPC.Types
    ( Port
    , CompactInfo (..)
    , Message     (..)
    , QueryDat    (..)
    , ResponseDat (..)
    , NodeInfo    (..)
    , NodeID
    )
import Mainline.Mainline
    ( Model (..)
    , ServerConfig (..)
    , Action (..)
    , TransactionState (..)
    , Transactions
    )

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
tidsize = 2

createInitialState :: NodeID -> Model
createInitialState newNodeId =
    ServerState
        { transactions = Map.empty
        , conf = ServerConfig
                { listenPort = servePort
                , seedNode = seedNodeInfo
                , ourId = newNodeId
                }
        , routingTable = initRoutingTable newNodeId
        }

data Msg
    = NewNodeId BS.ByteString
    | Inbound POSIXTime CompactInfo KPacket
    | ErrorParsing CompactInfo BS.ByteString String
    | SendFirstMessage
        { sendRecipient :: CompactInfo
        , body          :: Message
        , newtid        :: BS.ByteString
        }
    | SendMessage
        { sendAction    :: Action
        , targetNode    :: NodeInfo
        , body          :: Message
        , newtid        :: BS.ByteString
        , when          :: POSIXTime
        }


init :: (Model, Cmd.Cmd Msg)
init = (Uninitialized, Cmd.randomBytes 20 NewNodeId)


update :: Msg -> Model -> (Model, Cmd.Cmd Msg)
-- Error Parsing
update (ErrorParsing compactinfo bytes errmsg) model = (model, logmsg)
    where
        logmsg = Cmd.log Cmd.INFO $
            [ "Could not parse received message. Sender:" , show (compactinfo)
            , "in:" , show bytes
            ]
            ++ scnr ++
            [ "reason:", errmsg
            ]

        scnr = either
            (\_ -> [])
            (\bval -> ["scanner:", show $ scanner bval])
            (decode bytes)

-- Get new node id. Init server state, request tid for pinging seed node
update (NewNodeId bs) Uninitialized = (initState, initialCmds)
    where
        initialCmds = Cmd.batch [ logmsg, pingSeed ]
        initState = Uninitialized1 ourid

        pingSeed =
            prepareMsg
                (seedNode (conf initState))
                (Query ourid Ping)

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
    (ServerState { transactions, conf, routingTable }) =
        ( ServerState { transactions, conf, routingTable }
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
    (ServerState { transactions, conf, routingTable }) =
        ( ServerState
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
                , "(" ++ show bvalue ++ ") to", show targetNode ]


update
    ( Inbound
        now
        client
        ( KPacket
            { transactionId
            , message = Response nodeid Pong
            , version
            }
        )
    )
    (Uninitialized1 ourid) = (model, warmup)
        where
            model = createInitialState ourid

            warmup = prepareMsg2
                now
                Warmup
                (NodeInfo nodeid client)
                (Query ourid (FindNode ourid))


-- Receive a message
update
    (Inbound now client (KPacket { transactionId, message, version }))
    (ServerState { transactions, conf, routingTable }) =
        ( newModel
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
            mtState
                = nodeid
                >>= \nid -> Map.lookup nid transactions
                >>= \trns -> Map.lookup transactionId trns

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
                    Map.adjust
                        (Map.delete transactionId)
                        nid
                        transactions
                    )
                , conf
                , routingTable
                }

            (newModel, cmd) = case message of
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

respond
    :: NodeInfo
    -> BS.ByteString
    -> POSIXTime
    -> QueryDat
    -> Model
    -> (Model, Cmd.Cmd Msg)
-- Respond to Ping
respond
    node
    transactionId
    now
    Ping
    model
    | exists (routingTable model) node = (model, pong)
    | willAdd (routingTable model) node =
        (model { routingTable = rt }, Cmd.batch [log, cmds, pong])
    | otherwise = (model, pong)
        where
            pong =
                Cmd.sendUDP
                    (listenPort (conf model))
                    (compactInfo node)
                    (BL.toStrict bvalue)

            kpacket = KPacket transactionId (Response ourid Pong) Nothing
            ourid = (ourId (conf model))
            (rt, cmds) = considerNode now (routingTable model) node
            log = Cmd.log Cmd.DEBUG [ "sending", show bvalue]
            bvalue = encode kpacket



handleResponse
    :: NodeInfo
    -> TransactionState
    -> POSIXTime
    -> ResponseDat
    -> Model
    -> (Model, Cmd.Cmd Msg)
-- Respond to FindNode Response during Warmup
handleResponse
    node
    (TransactionState _ Warmup _)
    now
    (Nodes ninfos)
    (ServerState { transactions, conf, routingTable })
    | willAdd routingTable node =
        (ServerState transactions conf newrt, cmds)
    | otherwise = (ServerState { transactions, conf, routingTable }, Cmd.none)
        where
            logmsg = Cmd.log Cmd.INFO [ "Adding to routing table:", show node ]
            rt = uncheckedAdd
                    routingTable
                    (Node now node Normal)

            cmds = Cmd.batch $ [ logmsg ] ++ sendCmds

            (newrt, sendCmds) =
                foldl
                    ( \(rt_, l) nodeinfo ->
                        (\(rt__, cmd) -> (rt__, l ++ [cmd]))
                        (considerNode now rt_ nodeinfo)
                    )
                    (rt, [])
                    ( filter
                        (filterNodes transactions (ourId conf))
                        (fromAscList ninfos)
                    )


logHelper :: NodeInfo -> Message -> String -> Model -> (Model, Cmd.Cmd Msg)
logHelper sender msg logstr model  = (model, log)
            where
                log = Cmd.log Cmd.INFO
                    [ logstr
                    , show sender
                    , show msg
                    ]


logErr :: CompactInfo -> Message -> Model -> (Model, Cmd.Cmd Msg)
logErr
    sender
    (Error { errCode, errMsg })
    model = (model, log)
        where
            log = Cmd.log Cmd.INFO
                [ "Received an Error from"
                , show sender
                , (show errCode) ++ ": " ++ show errMsg
                ]

logErr _ _ model = (model, Cmd.none)


filterNodes :: Transactions -> NodeID -> NodeInfo -> Bool
filterNodes _ ourid node
    =  (1 <= p)
    && (p <= 65535)
    && nodeId node /= ourid

    where
        p = (port $ compactInfo node)



-- Node has not yet been contacted
considerNode
    :: POSIXTime
    -> RoutingTable
    -> NodeInfo
    -> (RoutingTable, Cmd.Cmd Msg)
considerNode now rt nodeinfo
    | exists rt nodeinfo =  (rt, Cmd.none)
    | willAdd rt nodeinfo = (rt, pingThem)
    | otherwise =           (rt, Cmd.none)
        where
            pingThem =
                prepareMsg2
                    now
                    Warmup
                    nodeinfo
                    (Query ourid (FindNode ourid))
            ourid = getOwnId rt
    -- node exists in rt => (Model, Cmd.none)
    -- node can be added (bucket not full or ourid in bucket) => send message
    -- if nodes where lastMsgTime < (now - 15m) || status == BeingChecked
    --      then send command or modify TransactionState


-- This is used for when we want to send a message but do not have the current
-- time.
prepareMsg :: CompactInfo -> Message -> Cmd.Cmd Msg
prepareMsg compactinfo body =
        Cmd.randomBytes
        tidsize
        (\t -> SendFirstMessage
            { sendRecipient = compactinfo
            , body          = body
            , newtid        = t
            }
        )

prepareMsg2 :: POSIXTime -> Action -> NodeInfo -> Message -> Cmd.Cmd Msg
prepareMsg2 now action node body =
    Cmd.randomBytes
    tidsize
    (\newid -> SendMessage
        { sendAction = action
        , targetNode = node
        , body       = body
        , newtid     = newid
        , when       = now
        }
    )



parseReceivedBytes :: CompactInfo -> Received -> Msg
parseReceivedBytes compactinfo (Received { bytes, time }) =
    case decode bytes of
        Right kpacket -> Inbound time compactinfo kpacket
        Left msg -> ErrorParsing compactinfo bytes msg



subscriptions :: Model -> Sub Msg
subscriptions Uninitialized = Sub.none
subscriptions (ServerState { conf = ServerConfig { listenPort } }) =
    Sub.udp listenPort parseReceivedBytes


config :: Config Model Msg
config = Config init update subscriptions


main :: IO ()
main = run config
