{-# LANGUAGE NamedFieldPuns #-}

module Architecture.Internal.Sub
    ( TSub (..)
    , Sub (..)
    , SubscriptionData (..)
    , Received (..)
    , updateSubscriptions
    , readSubscriptions
    , openUDPPort
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
    , close
    , accept
    , hostAddressToTuple
    )
import Network.Socket.ByteString (recv, recvFrom)
import Data.Hashable
import qualified Data.ByteString as BS
import Data.Time.Clock.POSIX (getPOSIXTime)

import Architecture.Internal.Types
    ( SubscriptionData (..)
    , Received (..)
    , SubState
    )
import Network.KRPC.Types (Port, CompactInfo (CompactInfo))
import Network.Octets (fromOctets)

import Debug.Trace (trace)

--MAXLINE = 65507 -- Max size of a UDP datagram
--(limited by 16 bit length part of the header field)
maxline :: Int
maxline = 2048


data TSub msg
    = TCP Port (BS.ByteString -> msg)
    | UDP Port (CompactInfo -> Received -> msg)

instance Hashable (TSub msg) where
    hashWithSalt s (TCP p _) = s `hashWithSalt` (0 :: Int) `hashWithSalt` p
    hashWithSalt s (UDP p _) = s `hashWithSalt` (1 :: Int) `hashWithSalt` p


newtype Sub msg = Sub [ TSub msg ]


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

        unsub (UDPDat { boundSocket }) = do
            trace "closing bound udp socket" close boundSocket

        sub :: TSub msg -> IO (SubscriptionData msg)
        sub (TCP p f) = do
            sock <- openTCPPort p
            return $ TCPDat p sock Nothing f

        sub (UDP p f) = do
            sock <- trace "opening udp port" (openUDPPort p)
            return $ UDPDat p sock f

        unloads = substates `Map.withoutKeys` (Set.fromList tsubkeys)

        foldSubs :: SubState msg -> (Int, TSub msg) -> IO (SubState msg)
        foldSubs s (k, t) =
            sub t >>= (\s_ -> (return $ Map.insert k s_ s))

        loads =
            filter (\(k, _) -> Map.notMember k substates) tsubpairs

        tsubpairs = [(k, t) | k <- tsubkeys, t <- tsubs]

        tsubkeys = map hash tsubs

updateHandlers :: SubState msg -> [ (Int, TSub msg) ] -> SubState msg
updateHandlers s tsubs = foldl something Map.empty tsubs
    where
        something s_ (key, tsub) =
            maybe s_ (\sub ->
                Map.insert key (updateHandler tsub sub) s_) (Map.lookup key s)

        updateHandler
            (TCP _ f)
            (TCPDat {port, listenSocket, connectedSocket}) =
                TCPDat {port, listenSocket, connectedSocket, tcpHandler=f}

        updateHandler
            (UDP _ f)
            (UDPDat {port, boundSocket}) =
                UDPDat {port, boundSocket, udpHandler=f}

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


openTCPPort :: Port -> IO Socket
openTCPPort p = do
    sock <- bindSocket Stream p
    listen sock 5
    return sock


openUDPPort :: Port -> IO Socket
openUDPPort = bindSocket Datagram


readSubscriptions :: SubState msg -> IO (SubState msg, [ msg ])
readSubscriptions x = trace ("readSubscriptions size x: " ++ (show $ Map.size x) ++ "  size assocs:" ++ (show $ length $ Map.assocs x)) $ ((foldM ff (Map.empty, [])) . Map.assocs) x
    where
        ff ::
            (SubState msg, [ msg ]) ->
            (Int, SubscriptionData msg) ->
            IO (SubState msg, [ msg ])
        ff (states, msgs) (key, value) = trace "readSubscriptions.ff" (
            readSub value >>=
                \(d, mmsg) ->
                    return (Map.insert key d states, maybe msgs (: msgs) mmsg))


readSub :: SubscriptionData msg -> IO (SubscriptionData msg, Maybe msg)
readSub (TCPDat p lsnsc cnsc f) =
    do
        closem cnsc

        (c, _) <- accept $ lsnsc
        bytes <- trace "reading in from tcp socket" (recvAll BS.empty c)
        return (TCPDat p lsnsc (Just c) f, Just $ f bytes)

    where
        recvAll :: BS.ByteString -> Socket -> IO BS.ByteString
        recvAll bss sock = do
            bs <- recv sock 4096

            case BS.length bs of
                0 ->  return bss
                _ -> recvAll (bss `BS.append` bs) sock

readSub (UDPDat { port, boundSocket, udpHandler }) = do
    putStrLn $ "reading udp socket, port " ++ show port
    (bs, sockaddr) <- recvFrom boundSocket maxline
    putStrLn $ "received " ++ (show $ BS.length bs) ++ " bytes from udp socket"
    now <- getPOSIXTime

    return
        ( UDPDat
            port
            boundSocket
            udpHandler
        , Just $ udpHandler
            (addrToCi sockaddr)
            (Received bs now)
        )

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
