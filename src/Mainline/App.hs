{-# LANGUAGE NamedFieldPuns #-}

import Prelude hiding (init, log)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map             as Map
import Data.Word                     (Word32)
import Data.Maybe                    (isJust)
import Data.BEncode                  (encode, decode, BValue)
import Data.Time.Clock.POSIX         (POSIXTime)

import Architecture.TEA              (Config (..), run)
import qualified Architecture.Cmd as Cmd
import qualified Architecture.Sub as Sub
import Architecture.Sub              (Sub, Received (..))
import Network.KRPC                  (KPacket (..), scanner)
import Network.KRPC.Helpers          (hexify)
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

writeDataRoot :: String
writeDataRoot = "/tmp/dht_out2/"

receivedResponseDump :: String
receivedResponseDump = writeDataRoot ++ "findNodes_response.out"

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
    | Inbound POSIXTime CompactInfo KPacket BS.ByteString
    | ErrorParsing CompactInfo BS.ByteString String
    | GetTime Msg
    | GotTime Msg POSIXTime
    | SendMessage
        { sendAction    :: Action
        , sendRecipient :: CompactInfo
        , body          :: Message
        , newtid        :: BS.ByteString
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
        initState = createInitialState (fromByteString bs)

        pingSeed =
            prepareMsg
                Warmup
                (seedNode (conf initState))
                (Query ourid Ping)

        ourid = ourId (conf initState)

        logmsg = Cmd.log Cmd.DEBUG
            [ "Initializing with node id:"
            , hexify $ octets (ourId (conf initState))
            ]


update (GetTime msg) state = (state, Cmd.getTime (GotTime msg))

-- Get tid for outound Message
update
    ( GotTime
        ( SendMessage
            { sendAction
            , sendRecipient
            , body
            , newtid
            }
        )
        now
    )
    ( ServerState { transactions, conf, routingTable }) =
        ( ServerState
            { transactions = trsns
            , conf
            , routingTable
            }
        , Cmd.batch [ logmsg, logmsg2, sendCmd ]
        )

        where
            trsns =
                Map.insert
                    newtid
                    ( TransactionState
                        { timeSent = now
                        , action = sendAction
                        , recipient = sendRecipient
                        }
                    )
                    transactions

            kpacket = KPacket newtid body Nothing

            sendCmd =
                Cmd.sendUDP
                    (listenPort conf)
                    sendRecipient
                    (BL.toStrict bvalue)

            bvalue = encode kpacket

            logmsg = Cmd.log Cmd.DEBUG
                [ "Sending", show kpacket
                , "(" ++ show bvalue ++ ") to", show sendRecipient ]

            logmsg2 = Cmd.log Cmd.DEBUG [ "tid size:", show $ BS.length $ newtid]


-- Receive a message
update
    (Inbound now client (KPacket { transactionId, message, version }) bytes)
    (ServerState { transactions, conf, routingTable }) =
        ( newModel
        , Cmd.batch
            [ Cmd.log
                Cmd.DEBUG
                [ "IN from " , show client
                , " received:\n" , show kpacket
                , "(" ++ show (encode kpacket) ++ ")"
                ]
            , Cmd.log Cmd.DEBUG
                [ "Transaction found in state: "
                , show $ isJust mtState
                ]
            , maybe
                Cmd.none
                (\t -> if (recipient t) == client then
                        Cmd.none
                    else
                        Cmd.log Cmd.WARNING
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
            mtState = Map.lookup transactionId transactions
            kpacket = KPacket { transactionId, message, version }

            (newModel, cmd) = respond
                client
                (maybe (Left transactionId) Right mtState)
                now
                message
                ( ServerState
                    { transactions =
                        Map.delete transactionId transactions
                    , conf
                    , routingTable
                    }
                )
                bytes


respond
    :: CompactInfo
    -> Either BS.ByteString TransactionState
    -> POSIXTime
    -> Message
    -> Model
    -> BS.ByteString
    -> (Model, Cmd.Cmd Msg)
-- Respond to Ping
respond
    sender
    (Left transactionId)
    now
    (Query nodeid Ping)
    model
    _
    | exists (routingTable model) nodeinfo = (model, pong)
    | willAdd (routingTable model) nodeinfo =
        (model { routingTable = rt }, Cmd.batch [log, cmds, pong])
    | otherwise = (model, pong)
        where
            pong =
                Cmd.sendUDP
                    (listenPort (conf model))
                    sender
                    (BL.toStrict bvalue)

            kpacket = KPacket transactionId (Response ourid Pong) Nothing
            ourid = (ourId (conf model))
            (rt, cmds) = considerNode now (routingTable model) nodeinfo
            nodeinfo = NodeInfo nodeid sender
            log = Cmd.log Cmd.DEBUG [ "sending", show bvalue]
            bvalue = encode kpacket


-- Handle Pong Response during Warmup
respond
    sender
    (Right (TransactionState { action = Warmup }))
    now
    (Response nodeid (Pong))
    model
    _
    | exists rt nodeinfo  = (model, Cmd.none)
    | willAdd rt nodeinfo = (model, findUs)
    | otherwise           = (model, Cmd.none)
        where
            findUs =
                prepareMsg2
                    now
                    Warmup
                    sender
                    (Query ourid (FindNode ourid))

            rt = routingTable model
            nodeinfo = NodeInfo nodeid sender
            ourid = (ourId (conf model))

-- Respond to FindNode Response during Warmup
respond
    sender
    (Right (TransactionState { action = Warmup }))
    now
    (Response nodeid (Nodes ninfos))
    (ServerState { transactions, conf, routingTable })
    bytes
    | willAdd routingTable node =
        (ServerState transactions conf newrt, cmds)
        --(Uninitialized, cmds)
    | otherwise = (ServerState { transactions, conf, routingTable }, Cmd.none)
        where
            logmsg = Cmd.log Cmd.INFO [ "Adding to routing table:", show node ]
            node = NodeInfo nodeid sender
            rt = uncheckedAdd
                    routingTable
                    (Node now node Normal)

            cmds = Cmd.batch $
                [ logmsg
                , Cmd.writeFile receivedResponseDump bytes
                , Cmd.log Cmd.INFO (map show ninfos)
                ] ++ sendCmds

            (newrt, sendCmds) =
                foldl
                    ( \(rt_, l) nodeinfo ->
                        (\(rt__, cmd) -> (rt__, l ++ [cmd]))
                        (considerNode now rt_ nodeinfo)
                    )
                    (rt, [])
                    ninfos


-- Node has not yet been contacted
considerNode
    :: POSIXTime
    -> RoutingTable
    -> NodeInfo
    -> (RoutingTable, Cmd.Cmd Msg)
considerNode now rt nodeinfo
    | exists rt nodeinfo = (rt, Cmd.none)
    | willAdd rt nodeinfo = (rt, pingThem)
    | otherwise = (rt, Cmd.none)
        where
            pingThem =
                prepareMsg2
                    now
                    Warmup
                    (compactInfo nodeinfo)
                    (Query ourid (FindNode ourid))
            ourid = getOwnId rt
    -- node exists in rt => (Model, Cmd.none)
    -- node can be added (bucket not full or ourid in bucket) => send message
    -- if nodes where lastMsgTime < (now - 15m) || status == BeingChecked
    --      then send command or modify TransactionState


-- This is used for when we want to send a message but do not have the current
-- time.
prepareMsg :: Action -> CompactInfo -> Message -> Cmd.Cmd Msg
prepareMsg action compactinfo body =
        Cmd.randomBytes
        tidsize
        (\t -> GetTime SendMessage
            { sendAction    = action
            , sendRecipient = compactinfo
            , body          = body
            , newtid        = t
            }
        )

prepareMsg2 :: POSIXTime -> Action -> CompactInfo -> Message -> Cmd.Cmd Msg
prepareMsg2 now action compactinfo body =
    Cmd.randomBytes -- we are here.
    --We need this to be deterministic and to match the python test ✓.
    --Then write the payload to a known file instead of sending it.
    tidsize
    (\newid -> GotTime SendMessage
        { sendAction    = action
        , sendRecipient = compactinfo
        , body          = body
        , newtid        = newid
        }
        now
    )



parseReceivedBytes :: CompactInfo -> Received -> Msg
parseReceivedBytes compactinfo (Received { bytes, time }) =
    case decode bytes of
        Right kpacket -> Inbound time compactinfo kpacket bytes
        Left msg -> ErrorParsing compactinfo bytes msg



subscriptions :: Model -> Sub Msg
subscriptions Uninitialized = Sub.none
subscriptions (ServerState { conf = ServerConfig { listenPort } }) =
    Sub.udp listenPort parseReceivedBytes


config :: Config Model Msg
config = Config init update subscriptions


main :: IO ()
main = run config
