{-# LANGUAGE NamedFieldPuns #-}

import Prelude hiding (init)
import Network.Socket (SockAddr (..))
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import Data.Word                     (Word32)
import Data.Maybe                    (isJust)
import Data.Digest.SHA1              (Word160 (..))
import Data.BEncode                  (encode, decode)

import Architecture.TEA              (Config (..), run)
import qualified Architecture.Cmd as Cmd
import qualified Architecture.Sub as Sub
import Architecture.Sub (Sub)
import Network.KRPC                  (KPacket (..))
import Network.KRPC.Helpers          (hexify)
import Network.Octets                (Octets (..), fromByteString)
import Mainline.Bucket               (RoutingTable (Bucket))
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

import Debug.Trace (trace)

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

--Maximum size of a bucket in the Routing Table before it must be split
bucketsize :: Int
bucketsize = 8

createInitialState :: NodeID -> Model
createInitialState ourNodeId =
    ServerState
        { transactionState = Map.empty
        , conf = ServerConfig
                { listenPort = servePort
                , seedNode = seedNodeInfo
                , nodeid = ourNodeId
                }
        , routingTable = rt
        }
    where
        rt = Bucket ourNodeId bucketsize minword maxword Set.empty
        minword = Word160 i i i i i
        maxword = Word160 j j j j j
        i = minBound :: Word32
        j = maxBound :: Word32

data Msg
    = NewNodeId BS.ByteString
    | ReceiveBytes CompactInfo (Maybe KPacket)
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
    (ServerState { transactionState, conf, routingTable }) =
        ( ServerState
            { transactionState = ts
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
                transactionState

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
    (ReceiveBytes client (Just (KPacket { transactionId, message })))
    (ServerState { transactionState, conf, routingTable }) =
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
            mtState = Map.lookup transactionId transactionState
            (newModel, cmd) = respond
                mtState
                ( ServerState
                    { transactionState
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
        Right kpacket ->
            trace "received bytes and parsed" $ ReceiveBytes compactinfo (Just kpacket)
        _ -> ReceiveBytes compactinfo Nothing

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
