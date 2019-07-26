{-# LANGUAGE NamedFieldPuns #-}

module Architecture.Internal.Sub
    ( SubscriptionData (..)
    , Received (..)
    , updateSubscriptions
    , readSubscriptions
    , openUDPPort
    , connectTCP
    , ciToAddr
    ) where

import Control.Monad (foldM)
import qualified Data.Map as Map
import qualified Data.Set as Set
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
    , listen
    , connect
    , close
    , accept
    , hostAddressToTuple
    , tupleToHostAddress
    )
import Network.Socket.ByteString (recv, recvFrom)
import System.Timeout (timeout)
import Data.Hashable (hash)
import qualified Data.ByteString as BS
import Data.Time.Clock.POSIX (getPOSIXTime)

import Architecture.Internal.Types
    ( SubscriptionData (..)
    , Received (..)
    , SubState
    , TSub (..)
    , Sub (..)
    )
import Network.Octets (octets)
import Network.KRPC.Types (Port, CompactInfo (CompactInfo))
import Network.Octets (fromOctets)

--MAXLINE = 65507 -- Max size of a UDP datagram
--(limited by 16 bit length part of the header field)
maxline :: Int
maxline = 1472 -- 1500 MTU - 20 byte IPv4 header - 8 byte UDP header

--In μs
udpTimeout :: Int
udpTimeout = 10 * (((^) :: Int -> Int -> Int) 10 6)

updateSubscriptions :: SubState msg -> Sub msg -> IO (SubState msg)
updateSubscriptions substates (Sub tsubs) = do
    mapM_ unsub unloads
    loaded <- foldM foldSubs Map.empty loads
    return $
        Map.union
            (updateHandlers (substates `Map.difference` unloads) tsubpairs)
            loaded

    where
        unsub :: SubscriptionData msg -> IO ()
        unsub (TCPDat { connectedSocket, listenSocket }) = do
            closem connectedSocket
            close listenSocket

        unsub (TCPClientDat { clientSocket }) = close clientSocket

        unsub (UDPDat { boundSocket }) = close boundSocket

        unsub (TimerDat _ _ _) = return ()

        sub :: TSub msg -> IO (SubscriptionData msg)
        sub (TCP p f) = do
            sock <- openTCPPort p
            return $ TCPDat p sock Nothing f

        sub (TCPClient ci g h) = do
            sock <- connectTCP ci
            return $ TCPClientDat ci sock g h

        sub (UDP p f) = do
            sock <- openUDPPort p
            return $ UDPDat p sock f

        sub (Timer dt f) = getPOSIXTime >>= return . (TimerDat dt f)

        unloads = substates `Map.withoutKeys` (Set.fromList tsubkeys)

        foldSubs :: SubState msg -> (Int, TSub msg) -> IO (SubState msg)
        foldSubs s (k, t) = do
            sub t >>= (\s_ -> (return $ Map.insert k s_ s))

        loads =
            filter (\(k, _) -> Map.notMember k substates) tsubpairs

        tsubpairs = zip tsubkeys tsubs

        tsubkeys = map hash tsubs

updateHandlers :: SubState msg -> [ (Int, TSub msg) ] -> SubState msg
updateHandlers s tsubs = foldl something Map.empty tsubs
    where
        something s_ (key, tsub) =
            maybe s_ (\sub ->
                Map.insert key (updateHandler tsub sub) s_) (Map.lookup key s)

        updateHandler
            (TCP _ f)
            (TCPDat { port, listenSocket, connectedSocket }) =
                TCPDat { port, listenSocket, connectedSocket, tcpHandler=f }

        updateHandler
            (UDP _ f)
            (UDPDat { port, boundSocket }) =
                UDPDat { port, boundSocket, udpHandler=f }

        updateHandler
            (Timer _ f)
            (TimerDat { ms, lastTime }) =
                TimerDat ms f lastTime

        updateHandler
            (TCPClient _ g h)
            (TCPClientDat { info, clientSocket }) =
                TCPClientDat { info, clientSocket, getMore=g, clientHandler=h }

        updateHandler _  _ = undefined


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


-- Used by servers
openTCPPort :: Port -> IO Socket
openTCPPort p = do
    sock <- bindSocket Stream p
    listen sock 5
    return sock


-- Used by clients
connectTCP :: CompactInfo -> IO Socket
connectTCP ci = do
    sock <- socket AF_INET Stream defaultProtocol
    connect sock (ciToAddr ci)
    return sock


openUDPPort :: Port -> IO Socket
openUDPPort = bindSocket Datagram


readSubscriptions :: SubState msg -> IO (SubState msg, [ msg ])
readSubscriptions = (foldM ff (Map.empty, [])) . Map.assocs
    where
        ff ::
            (SubState msg, [ msg ]) ->
            (Int, SubscriptionData msg) ->
            IO (SubState msg, [ msg ])
        ff (states, msgs) (key, value)
            = readSub value
            >>= \(d, mmsg) ->
                return (Map.insert key d states, maybe msgs (: msgs) mmsg)


readSub :: SubscriptionData msg -> IO (SubscriptionData msg, Maybe msg)
readSub (TCPDat p lsnsc cnsc f) =
    do
        closem cnsc

        (c, _) <- accept $ lsnsc
        bytes <- recvAll BS.empty c
        return (TCPDat p lsnsc (Just c) f, Just $ f bytes)

    where
        recvAll :: BS.ByteString -> Socket -> IO BS.ByteString
        recvAll bss sock = do
            bs <- recv sock 4096

            case BS.length bs of
                0 ->  return bss
                _ -> recvAll (bss `BS.append` bs) sock

readSub (TCPClientDat ci sock g h) = do
    bytes <- more sock g BS.empty
    now <- getPOSIXTime
    return (TCPClientDat ci sock g h, Just $ h $ Received bytes now)

    where
        more s f msg =
            let n = f msg in
            if n < 1 then return msg
            else do
                bs <- recv s n
                more s f (msg <> bs)


readSub (UDPDat { port, boundSocket, udpHandler }) =
    (timeout udpTimeout (recvFrom boundSocket maxline))
    >>= maybe (return (subdata, Nothing)) handle

    where
        subdata = UDPDat port boundSocket udpHandler

        handle (bs, sockaddr) = do
            now <- getPOSIXTime

            return
                ( subdata
                , Just $ udpHandler
                    (addrToCi sockaddr)
                    (Received bs now)
                )

readSub (TimerDat {ms, timerHandler, lastTime}) = do
    now <- getPOSIXTime
    if (now - lastTime) < (fromIntegral ms) / 1000
    then
        return (TimerDat ms timerHandler lastTime, Nothing)
    else
        return (TimerDat ms timerHandler now, Just $ timerHandler now)


closem :: Maybe Socket -> IO ()
closem = maybe (return ()) close


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

