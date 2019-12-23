module Architecture.Internal.Network
    ( openUDPPort
    , connectTCP
    , addrToCi
    , ciToAddr
    , openSocket
    ) where

import Network.Socket
    ( Socket
    , socket
    , SocketType (Stream, Datagram)
    , SockAddr (..)
    , Family (AF_INET)
    , AddrInfo (addrFlags, addrFamily, addrSocketType, addrAddress)
    , AddrInfoFlag (AI_PASSIVE)
    , defaultProtocol
    , defaultHints
    , getAddrInfo
    , bind
    --, listen
    , connect
    , close
    , hostAddressToTuple
    , tupleToHostAddress
    )
import Control.Concurrent.STM (TQueue, atomically, writeTQueue)
import Control.Exception.Safe (catchIO, onException)

import Network.KRPC.Types (Port, CompactInfo (CompactInfo))
import Network.Octets (fromOctets, octets)
import Architecture.Internal.Types(TCmd (..))

openUDPPort :: Port -> IO Socket
openUDPPort = bindSocket Datagram

bindSocket :: SocketType -> Port -> IO Socket
bindSocket t p = do
    addr:_ <- getAddrInfo (Just hints) Nothing (Just $ show p)
    sock <- socket AF_INET t defaultProtocol
    bind sock (addrAddress addr) `onException` close sock
    return sock

    where
        hints :: AddrInfo
        hints = defaultHints
            { addrFlags = [AI_PASSIVE]
            , addrFamily = AF_INET
            , addrSocketType = t
            }

addrToCi :: SockAddr -> CompactInfo
addrToCi (SockAddrInet port host) =
    CompactInfo
        (fromOctets
            $ (\(a1, a2, a3, a4) -> [a1, a2, a3, a4])
            $ hostAddressToTuple host)
        (fromIntegral port)

addrToCi _ = undefined

ciToAddr :: CompactInfo -> SockAddr
ciToAddr (CompactInfo ip p) = SockAddrInet
    (fromIntegral p)
    (tupleToHostAddress
        $ (\[a1, a2, a3, a4] -> (a1, a2, a3, a4))
        $ octets ip)


-- Used by servers (Sub to serve on TCP removed atm)
--openTCPPort :: Port -> IO Socket
--openTCPPort p = do
--    sock <- bindSocket Stream p
--    listen sock 5
--    return sock


-- Used by clients
connectTCP :: CompactInfo -> IO Socket
connectTCP ci = do
    sock <- socket AF_INET Stream defaultProtocol
    connect sock (ciToAddr ci) `onException` close sock
    return sock


openSocket
    :: Int
    -> IO Socket
    -> TQueue (TCmd msg schemas)
    -> IO ()
    -> IO ()
openSocket key getSock cmdSink onError = do
    msock <- catchIO
                (getSock >>= return . Just)
                (\_ -> return Nothing)

    case msock of
        Just sock ->
            atomically $ writeTQueue cmdSink (SocketResult key (Just sock))
        Nothing -> do
            atomically $ writeTQueue cmdSink (SocketResult key Nothing)
            onError

