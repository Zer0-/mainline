{-
 - Testing sending and receiving a single UDP packet
 -}

--import KRPC
import Network.Socket
    ( socket
    , sendTo
    , recvFrom
--  , bind
    , Family(AF_INET)
    , HostAddress
    , SocketType(Datagram)
    , SockAddr(..)
    , AddrInfo(..)
    , getAddrInfo
    , inet_ntoa
--  , iNADDR_ANY
    )

targetPort = "49000"
targetHost = "127.0.0.1"
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

main :: IO ()
main = do
    putStrLn "What would you like to send?"
    payload <- getLine
    sock           <- socket AF_INET Datagram 0
    addrInfoList   <- getAddrInfo Nothing (Just targetHost) (Just targetPort)
    let targetInetAddr = addrAddress (head addrInfoList)
    _ <- sendTo sock payload targetInetAddr
    (response, nBytesRecv, fromSocketAddr) <- recvFrom sock maxline
    fromHost <- hostFromSockAddr fromSocketAddr
    let fromPort = portFromSockAddr fromSocketAddr
    putStrLn $ "RESPONSE ("
                ++ (show nBytesRecv)
                ++ " bytes from " ++ (show fromHost) ++ ":" ++ (show fromPort) ++ ")\n"
                ++ response
