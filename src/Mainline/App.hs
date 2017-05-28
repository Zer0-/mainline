{-
 - Testing sending and receiving a single UDP packet
 -}

import Network.Socket
    ( Socket
    , socket
--  , sendTo
--  , bind
    , Family(AF_INET)
    , SocketType(Datagram)
    , SockAddr(..)
--  , iNADDR_ANY
    )

import Network.Socket.ByteString (sendTo, recvFrom)
import Data.Word (Word32)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.BEncode (encode, decode)
import Data.Digest.SHA1 (Word160)
import qualified Data.Map as Map
import Crypto.Random (newGenIO, genBytes)
import Crypto.Random.DRBG (CtrDRBG)

import Network.KRPC         (KPacket (..))
import Network.KRPC.Types   (Port, CompactInfo (..), Message (..))
import Network.KRPC.Helpers (stringpack)
import Network.Octets       (Octets (..), fromByteString)
import Mainline.Mainline
    ( ServerState (..)
    , Outbound
    , serverHandler
    , NotImplemented (..)
    )

seedNodePort :: Port
seedNodePort = 51413

seedNodeHost :: Word32
seedNodeHost = fromOctets [192, 168, 4, 11]

seedNodeInfo :: CompactInfo
seedNodeInfo = CompactInfo seedNodeHost seedNodePort

--MAXLINE = 65507 -- Max size of a UDP datagram
--(limited by 16 bit length part of the header field)
maxline :: Int
maxline = 2048

tidsize :: Int
tidsize = 128

word160FromString :: String -> Word160
word160FromString = fromByteString . stringpack

ping :: Message
ping = Query (word160FromString "hello world") Ping

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

    putStrLn $ "RESPONSE: " ++ (show $ kpacket)
        ++ " FROM: " ++ show senderInfo

    let (state3, outbounds2) = serverHandler state2 senderInfo kpacket
    mainloop sock state3 outbounds2


main :: IO ()
main = do
    sock <- socket AF_INET Datagram 0

    tid <- genTid
    mainloop sock (ServerState Map.empty) [(seedNodeInfo, Left $ KPacket tid ping)]
