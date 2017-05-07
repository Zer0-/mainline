{-
 - Testing sending and receiving a single UDP packet
 -}

import Network.Socket
    ( socket
--  , sendTo
--  , bind
    , Family(AF_INET)
--- , HostAddress
    , SocketType(Datagram)
    , SockAddr(..)
    , AddrInfo(..)
    , getAddrInfo
    , inet_ntoa
--  , iNADDR_ANY
    )

import Network.Socket.ByteString (sendTo, recvFrom)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.BEncode (encode)
import KRPC

targetPort = "51413"
targetHost = "192.168.4.11"
--MAXLINE = 65507 -- Max size of a UDP datagram
--(limited by 16 bit length part of the header field)
maxline = 2048

hostFromSockAddr :: SockAddr -> IO String
hostFromSockAddr (SockAddrInet _ h) = inet_ntoa h
hostFromSockAddr _ = undefined

portFromSockAddr :: SockAddr -> String
portFromSockAddr (SockAddrInet p _) = show p
portFromSockAddr (SockAddrInet6 p _ _ _) = show p
portFromSockAddr _ = undefined

ping = (BS.concat . BL.toChunks . encode)
    $ KPacket (stringpack "hello")
    $ Query ((fromOctets . BS.unpack . stringpack) "hello world") Ping

main :: IO ()
main = do
    --putStrLn "What would you like to send?"
    --payload <- getLine
    sock           <- socket AF_INET Datagram 0
    addrInfoList   <- getAddrInfo Nothing (Just targetHost) (Just targetPort)
    let targetInetAddr = addrAddress (head addrInfoList)
    _ <- sendTo sock ping targetInetAddr
    putStrLn $ "sent" ++ (show ping)
    (response, fromSocketAddr) <- recvFrom sock maxline
    fromHost <- hostFromSockAddr fromSocketAddr
    let fromPort = portFromSockAddr fromSocketAddr
    putStrLn $ "RESPONSE ("
                ++ (show $ BS.length response)
                ++ " bytes from " ++ (show fromHost) ++ ":" ++ (show fromPort) ++ ")\n"
                ++ show response
