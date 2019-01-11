{-# LANGUAGE NamedFieldPuns #-}

import Prelude hiding (init)
import Network.Socket (SockAddr (..))
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map             as Map
import Data.Word                     (Word32)
import Data.Maybe                    (isJust)
import Data.BEncode                  (encode, decode)
import Data.Time.Clock.POSIX         (POSIXTime)

import Architecture.TEA              (Config (..), run)
import qualified Architecture.Cmd as Cmd
import qualified Architecture.Sub as Sub
import Architecture.Sub (Sub, Received (..))
import Network.KRPC                  (KPacket (..))
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
    , Message (..)
    , NodeInfo (..)
    , NodeID
    )
import Mainline.Mainline
    ( Model (..)
    , ServerConfig (..)
    , Action (..)
    , TransactionState (..)
    )

servePort :: Port
servePort = 58080

seedNodePort :: Port
seedNodePort = 51413

seedNodeHost :: Word32
seedNodeHost = fromOctets [ 192, 168, 4, 2 ]

seedNodeInfo :: CompactInfo
seedNodeInfo = CompactInfo seedNodeHost seedNodePort

tidsize :: Int
tidsize = 8

createInitialState :: NodeID -> Model
createInitialState newNodeId =
    ServerState
        { transactions = Map.empty
        , conf = ServerConfig
                { listenPort = servePort
                , seedNode = seedNodeInfo
                , nodeid = newNodeId
                }
        , routingTable = initRoutingTable newNodeId
        }

data Msg
    = NewNodeId BS.ByteString
    | Inbound POSIXTime CompactInfo KPacket
    | ErrorParsing CompactInfo BS.ByteString
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
-- Get new node id. Init server state, request tid for pinging seed node
update (NewNodeId bs) Uninitialized = (initState, initialCmds)
    where
        initialCmds = Cmd.batch [ logmsg, pingSeed ]

        initState = createInitialState (fromByteString bs)

        pingSeed =
            prepareMsg
                ContactSeed
                (seedNode (conf initState))
                (Query ourid (FindNode ourid))

        ourid = nodeid (conf initState)

        logmsg = Cmd.log Cmd.DEBUG
            [ "Initializing with node id:"
            , hexify $ octets (nodeid (conf initState))
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
        , Cmd.batch [ logmsg, sendCmd ]
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

            kpacket = KPacket newtid body

            sendCmd =
                Cmd.sendUDP
                    (listenPort conf)
                    sendRecipient
                    (BL.toStrict (encode kpacket))

            logmsg = Cmd.log Cmd.DEBUG
                [ "Sending ", show kpacket ]


-- Receive a message
update
    (Inbound now client (KPacket { transactionId, message }))
    (ServerState { transactions, conf, routingTable }) =
        ( newModel
        , Cmd.batch
            [ Cmd.log
                Cmd.DEBUG
                [ "IN from " , show client
                , " received:\n" , show KPacket { transactionId, message }
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

            (newModel, cmd) = respond
                client
                mtState
                now
                message
                ( ServerState
                    { transactions =
                        Map.delete transactionId transactions
                    , conf
                    , routingTable
                    }
                )


respond
    :: CompactInfo
    -> Maybe TransactionState
    -> POSIXTime
    -> Message
    -> Model
    -> (Model, Cmd.Cmd Msg)
respond
    sender
    ( Just
        ( TransactionState
            { action = ContactSeed -- does this special case really exist?
            , recipient
            }
        )
    )
    now
    (Response nodeid (Nodes _))
    (ServerState { transactions, conf, routingTable }) =
        (ServerState transactions conf (uncheckedAdd routingTable (Node now (NodeInfo nodeid sender) Normal)), Cmd.none)


-- Node has not yet been contacted
considerNode :: POSIXTime -> RoutingTable -> NodeInfo -> (RoutingTable, Cmd.Cmd Msg)
considerNode now rt nodeinfo
    | exists rt nodeinfo = (rt, Cmd.none)
    | willAdd rt nodeinfo = (rt, findUs)
        where
            --a = --what do we need to add to the rt? Nothing!
            findUs =
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
    Cmd.randomBytes
    tidsize
    (\t -> GotTime SendMessage
        { sendAction    = action
        , sendRecipient = compactinfo
        , body          = body
        , newtid        = t
        }
        now
    )



compactInfoFromSockAddr :: SockAddr -> CompactInfo
compactInfoFromSockAddr (SockAddrInet port_ host) =
    CompactInfo host (fromIntegral port_)

compactInfoFromSockAddr _ = undefined


parseReceivedBytes :: SockAddr -> Received -> Msg
parseReceivedBytes fromNetinfo (Received { bytes, time }) =
    case decode bytes of
        Right kpacket -> Inbound time compactinfo kpacket
        _ -> ErrorParsing compactinfo bytes

    where
        compactinfo = compactInfoFromSockAddr fromNetinfo


subscriptions :: Model -> Sub Msg
subscriptions Uninitialized = Sub.none
subscriptions (ServerState { conf = ServerConfig { listenPort } }) =
    Sub.udp listenPort parseReceivedBytes


config :: Config Model Msg
config = Config init update subscriptions


main :: IO ()
main = run config
