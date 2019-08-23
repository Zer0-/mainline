module Architecture.Internal.Network
    ( openUDPPort
    , addrToCi
    , ciToAddr
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

import Network.KRPC.Types (Port, CompactInfo (CompactInfo))
import Network.Octets (fromOctets, octets)

openUDPPort :: Port -> IO Socket
openUDPPort = bindSocket Datagram

bindSocket :: SocketType -> Port -> IO Socket
bindSocket t p = do
    addr:_ <- getAddrInfo (Just hints) Nothing (Just $ show p)
    sock <- socket AF_INET t defaultProtocol
    bind sock (addrAddress addr)
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

