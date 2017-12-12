{-
 - Testing sending and receiving a single UDP packet
 -}

import Network.Socket
    ( Socket
    , socket
--  , sendTo
--  , bind
    , defaultProtocol
    , Family(AF_INET)
    , SocketType(Datagram)
    , SockAddr(..)
--  , iNADDR_ANY
    )

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import Network.Socket.ByteString     (sendTo, recvFrom)
import Data.Word                     (Word32)
import Data.Digest.SHA1              (Word160 (..))
import Data.BEncode                  (encode, decode)
import Crypto.Random                 (newGenIO, genBytes)
import Crypto.Random.DRBG            (CtrDRBG)

import Network.KRPC                  (KPacket (..))
import Network.KRPC.Helpers          (stringpack)
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
    , Outbound
    , serverHandler
    , NotImplemented (..)
    )

seedNodePort :: Port
seedNodePort = 51413

seedNodeHost :: Word32
seedNodeHost = fromOctets [192, 168, 4, 2]

seedNodeInfo :: CompactInfo
seedNodeInfo = CompactInfo seedNodeHost seedNodePort

--MAXLINE = 65507 -- Max size of a UDP datagram
--(limited by 16 bit length part of the header field)
maxline :: Int
maxline = 2048

tidsize :: Int
tidsize = 128

--Maximum size of a bucket in the Routing Table before it must be split
bucketsize :: Int
bucketsize = 8

word160FromString :: String -> Word160
word160FromString = fromByteString . stringpack

lazyBStoStrict :: BL.ByteString -> BS.ByteString
lazyBStoStrict = BS.concat . BL.toChunks

compactInfoFromSockAddr :: SockAddr -> Maybe CompactInfo 
compactInfoFromSockAddr (SockAddrInet port_ host) =
    Just $ CompactInfo host (fromIntegral port_)
compactInfoFromSockAddr _ = Nothing

sendKpacket :: Socket -> SockAddr -> KPacket -> IO Int
sendKpacket s a k = sendTo s (toBytes k) a
    where toBytes = lazyBStoStrict . encode

fireKPackets :: Socket -> [(CompactInfo, KPacket)] -> IO ()
fireKPackets _ [] = return ()
fireKPackets sock ((CompactInfo ip_ port_, kpacket):xs) = do

    putStrLn $ "sending: " ++ show kpacket ++ "\n  to " ++ show addr

    bytesSent <- sendKpacket sock addr kpacket

    putStrLn $ "sent " ++ show bytesSent ++ " bytes."

    fireKPackets sock xs
    where
        addr = SockAddrInet (fromIntegral $ port_) ip_


receive :: Socket -> IO (CompactInfo, KPacket)
receive sock = do
    (bytesIn, fromSocketAddr) <- recvFrom sock maxline

    case (compactInfoFromSockAddr fromSocketAddr, decode bytesIn) of
        (Just ci, Right kpacket) -> return (ci, kpacket)
        _                        -> receive sock


genTid :: IO (BS.ByteString)
genTid = do
    g <- newGenIO :: IO CtrDRBG
    case genBytes tidsize g of
        Left err -> error $ show err
        Right (result, _) -> return result


setTid :: ServerState -> Outbound -> IO (ServerState, (CompactInfo, KPacket))
setTid state (ci, Left kpacket) = return (state, (ci, kpacket))
setTid state (ci, Right (msg, NotImplemented)) =
    do
        newtid <- genTid
        let newTidState = Map.insert newtid NotImplemented (transactionState state)
        let newstate = (state { transactionState = newTidState })
        return (newstate, (ci, KPacket newtid msg))


setTids :: ServerState -> [Outbound] -> IO (ServerState, [(CompactInfo, KPacket)])
setTids state [] = return (state, [])
setTids state (x:xs) = do
    (state2, payload)  <- setTid state x
    (state3, payloads) <- setTids state2 xs
    return (state3, payload : payloads)


mainloop :: Socket -> ServerState -> [Outbound] -> IO ()
mainloop sock state outbounds = do
    (state2, payloads) <- setTids state outbounds
    fireKPackets sock payloads

    (senderInfo, kpacket) <- receive sock

    putStrLn $ "RECEIVED: " ++ (show $ kpacket)
        ++ " FROM: " ++ show senderInfo

    let (state3, outbounds2) = serverHandler state2 senderInfo kpacket
    mainloop sock state3 outbounds2


createInitialState :: NodeID -> ServerState
createInitialState ourNodeId = ServerState Map.empty ourNodeId rt
    where
        rt = Bucket ourNodeId bucketsize minword maxword Set.empty
        minword = Word160 i i i i i
        maxword = Word160 j j j j j
        i = minBound :: Word32
        j = maxBound :: Word32


main :: IO ()
main = do
    sock <- socket AF_INET Datagram defaultProtocol
    {-
     - Note that we never bind the socket. The obvious question is
     -          'what port are we listening on?'
     -
     - Apparently if we call sendTo before recvFrom the system will automatically
     - bind the socket for us. So we still don't know what port we're listening
     - on (should probably save that and print it).
     -
     - Source:
     - https://stackoverflow.com/a/14243544
     -}

    --TODO: Make one up
    let ourNodeId = word160FromString "hello world"

    let initialOutbound =
            ( seedNodeInfo
            , Right
                ( Query ourNodeId Ping
                , NotImplemented
                )
            )
    mainloop sock (createInitialState ourNodeId) [initialOutbound]
