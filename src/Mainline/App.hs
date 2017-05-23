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
    , inet_ntoa
--  , iNADDR_ANY
    )

import Network.Socket.ByteString (sendTo, recvFrom)

import Data.Word (Word32)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.BEncode (encode)
import Data.Digest.SHA1 (Word160)

import Network.KRPC         (KPacket (..))
import Network.KRPC.Types   (Port, CompactInfo (..), Message (..))
import Network.KRPC.Helpers (stringpack)
import Network.Octets       (Octets (..), fromByteString)
import Mainline.Mainline    (Outbound)

seedNodePort :: Port
seedNodePort = 51413

seedNodeHost :: Word32
seedNodeHost = fromOctets [192, 168, 4, 11]

--MAXLINE = 65507 -- Max size of a UDP datagram
--(limited by 16 bit length part of the header field)
maxline :: Int
maxline = 2048

hostFromSockAddr :: SockAddr -> IO String
hostFromSockAddr (SockAddrInet _ h) = inet_ntoa h
hostFromSockAddr _ = undefined

portFromSockAddr :: SockAddr -> String
portFromSockAddr (SockAddrInet p _) = show p
portFromSockAddr (SockAddrInet6 p _ _ _) = show p
portFromSockAddr _ = undefined

word160FromString :: String -> Word160
word160FromString = fromByteString . stringpack

ping :: KPacket
ping = KPacket (stringpack "hello")
    $ Query (word160FromString "hello world") Ping

lazyBStoStrict :: BL.ByteString -> BS.ByteString
lazyBStoStrict = BS.concat . BL.toChunks

sendKpacket :: Socket -> SockAddr -> KPacket -> IO Int
sendKpacket s a k = sendTo s (toBytes k) a
    where toBytes = lazyBStoStrict . encode

seedNodeInfo :: CompactInfo
seedNodeInfo = CompactInfo seedNodeHost seedNodePort

fireKPackets :: Socket -> Outbound -> IO ()
fireKPackets _ [] = return ()
fireKPackets sock ((kpacket, CompactInfo ip_ port_):xs) = do
    putStrLn $ "sending: " ++ show kpacket ++ "\n  to " ++ show addr
    bytesSent <- sendKpacket sock addr kpacket
    putStrLn $ "sent " ++ show bytesSent ++ " bytes."
    fireKPackets sock xs
    where
        addr = SockAddrInet (fromIntegral $ port_) ip_

--targetNodeAddr = SockAddrInet (fromIntegral $ port seedNodeInfo) (ip seedNodeInfo)

main :: IO ()
main = do
    sock           <- socket AF_INET Datagram 0

    fireKPackets sock [(ping, seedNodeInfo)]

    (response, fromSocketAddr) <- recvFrom sock maxline

    fromHost <- hostFromSockAddr fromSocketAddr
    let fromPort = portFromSockAddr fromSocketAddr
    putStrLn $ "RESPONSE ("
                ++ (show $ BS.length response)
                ++ " bytes from " ++ (show fromHost) ++ ":" ++ (show fromPort) ++ ")\n"
                ++ show response
