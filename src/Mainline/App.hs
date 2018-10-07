{-# LANGUAGE NamedFieldPuns #-}

import Prelude hiding (init)
import Network.Socket (SockAddr (..))
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import Data.Word                     (Word32)
import Data.Digest.SHA1              (Word160 (..))
import Data.BEncode                  (encode, decode)

import Architecture.TEA              (Config (..), run)
import qualified Architecture.Cmd as Cmd
import qualified Architecture.Sub as Sub
import Architecture.Sub (Sub)
import Network.KRPC                  (KPacket (..))
import Network.Octets                (Octets (..), fromByteString)
import Mainline.Bucket               (RoutingTable (Bucket))
import Network.KRPC.Types
    ( Port
    , CompactInfo (..)
    , Message (..)
    , NodeID
    )
import Mainline.Mainline
    ( ServerState (..)
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

createInitialState :: NodeID -> ServerState
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

init :: (ServerState, Cmd.Cmd Msg)
init = (Uninitialized, Cmd.randomBytes 160 NewNodeId)

update :: Msg -> ServerState -> (ServerState, Cmd.Cmd Msg)
update (NewNodeId bs) _ = (initState, initialCmds)
    where
        initialCmds = Cmd.batch [ logmsg, pingSeed ]

        initState = createInitialState (fromByteString bs)

        pingSeed =
            Cmd.randomBytes
            tidsize
            (\t -> SendMessage
                { sendAction = PingSeed
                , sendRecipient = seedNode (conf initState)
                , body = (Query (nodeid (conf initState)) Ping)
                , newtid = t
                }
            )

        logmsg = Cmd.log Cmd.DEBUG
            [ "Initializing with node id:", show bs ]

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

            sendCmd = Cmd.sendUDP (listenPort conf) sendRecipient (BL.toStrict (encode kpacket))

            logmsg = Cmd.log Cmd.DEBUG
                [ "Sending ", show kpacket ]

update
    (ReceiveBytes client (Just kpacket))
    (ServerState { transactionState }) =
        (Uninitialized, logmsg)

        where
            logmsg = Cmd.batch
                [ Cmd.log Cmd.DEBUG [ "IN from ", show client, " received:\n", show kpacket ]
                , Cmd.log Cmd.DEBUG
                    [ "Transaction found in state: "
                    , show $ Map.member (transactionId kpacket) transactionState
                    ]
                ]


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


subscriptions :: ServerState -> Sub Msg
subscriptions Uninitialized = Sub.none
subscriptions (ServerState { conf = ServerConfig { listenPort } }) =
    Sub.udp listenPort parseReceivedBytes


config :: Config ServerState Msg
config = Config init update subscriptions


main :: IO ()
main = run config
