{-# LANGUAGE NamedFieldPuns #-}

import Prelude hiding (init)
import Network.Socket (SockAddr (..))
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import Data.Word                     (Word32)
import Data.Maybe                    (isJust)
import Data.BEncode                  (encode, decode)

import Architecture.TEA              (Config (..), run)
import qualified Architecture.Cmd as Cmd
import qualified Architecture.Sub as Sub
import Architecture.Sub (Sub)
import Network.KRPC                  (KPacket (..))
import Network.KRPC.Helpers          (hexify)
import Network.Octets                (Octets (..), fromByteString)
import Mainline.RoutingTable         (initRoutingTable)
import Network.KRPC.Types
    ( Port
    , CompactInfo (..)
    , Message (..)
    , NodeID
    )
import Mainline.Mainline
    ( Model (..)
    , ServerConfig (..)
    , NotImplemented (..)
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
createInitialState ourNodeId =
    ServerState
        { transactions = Map.empty
        , conf = ServerConfig
                { listenPort = servePort
                , seedNode = seedNodeInfo
                , nodeid = ourNodeId
                }
        , routingTable = initRoutingTable ourNodeId
        }

data Msg
    = NewNodeId BS.ByteString
    | Inbound CompactInfo KPacket
    | ErrorParsing CompactInfo BS.ByteString
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
                PingSeed
                (seedNode (conf initState))
                (Query (nodeid (conf initState)) Ping)

        logmsg = Cmd.log Cmd.DEBUG
            [ "Initializing with node id:"
            , hexify $ octets (nodeid (conf initState))
            ]


-- Get tid for outound Message
update
    (SendMessage { sendAction, sendRecipient, body, newtid })
    (ServerState { transactions, conf, routingTable }) =
        ( ServerState
            { transactions = ts
            , conf
            , routingTable
            }
        , Cmd.batch [ logmsg, sendCmd ]
        )

        where
            ts =
                Map.insert
                    newtid
                    ( TransactionState
                        { timeSent = NotImplemented
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
    (Inbound client (KPacket { transactionId, message }))
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
                mtState
                ( ServerState
                    { transactions =
                        Map.delete transactionId transactions
                    , conf
                    , routingTable
                    }
                )


respond :: Maybe TransactionState -> Model -> (Model, Cmd.Cmd Msg)
respond (Just (TransactionState { action = PingSeed, recipient })) model =
    ( model
    , prepareMsg Warmup recipient (Query ourId (FindNode ourId))
    )

    where
        ourId = nodeid $ conf model


prepareMsg :: Action -> CompactInfo -> Message -> Cmd.Cmd Msg
prepareMsg action compactinfo body =
        Cmd.randomBytes
        tidsize
        (\t -> SendMessage
            { sendAction    = action
            , sendRecipient = compactinfo
            , body          = body
            , newtid        = t
            }
        )



compactInfoFromSockAddr :: SockAddr -> CompactInfo
compactInfoFromSockAddr (SockAddrInet port_ host) =
    CompactInfo host (fromIntegral port_)

compactInfoFromSockAddr _ = undefined


parseReceivedBytes :: SockAddr -> BS.ByteString -> Msg
parseReceivedBytes fromNetinfo bytes =
    case decode bytes of
        Right kpacket -> Inbound compactinfo kpacket
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
