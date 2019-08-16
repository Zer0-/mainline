{-# LANGUAGE NamedFieldPuns #-}

module Architecture.Internal.Sub
    ( Received (..)
    , updateSubscriptions
    , openUDPPort
    , connectTCP
    , ciToAddr
    ) where

import Prelude hiding (init)
import Control.Monad (foldM, forever)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
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
    --, accept
    , hostAddressToTuple
    , tupleToHostAddress
    )
import Network.Socket.ByteString (recv, recvFrom)
--import System.Timeout (timeout)
import Data.Hashable (hash)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Control.Concurrent (forkIO, ThreadId, threadDelay)
import Control.Concurrent.STM
    ( TVar
    , newTVar
    , atomically
    , readTVarIO
    , readTVar
    , writeTVar
    , putTMVar
    )

import Architecture.Internal.Types
    ( InternalState (..)
    , Config (..)
    , Received (..)
    , TSub (..)
    , Cmd
    , SubHandler (..)
    , Sub (..)
    )
import Architecture.Internal.Cmd (runCmds)
import Network.Octets (octets)
import Network.KRPC.Types (Port, CompactInfo (CompactInfo))
import Network.Octets (fromOctets)

--MAXLINE = 65507 -- Max size of a UDP datagram
--(limited by 16 bit length part of the header field)
maxline :: Int
maxline = 1472 -- 1500 MTU - 20 byte IPv4 header - 8 byte UDP header

--In μs
{-
udpTimeout :: Int
udpTimeout = 10 * (((^) :: Int -> Int -> Int) 10 6)
-}

updateSubscriptions
    :: Config model msg
    -> InternalState msg
    -> IO (InternalState msg)
updateSubscriptions cfg istate = do
    model <- readTVarIO $ fst $ init cfg
    let (Sub tsubs) = (subscriptions cfg) model

    (newsocks, loaded) <- foldM fsub (Map.empty, Map.empty) (loads tsubs)
    return istate
        { readThreadS = Map.union loaded (readThreadS istate)
        , sockets = newsocks
        }

    where
        loads =
            ( filter (\(k, _) -> Map.notMember k (readThreadS istate))
            ) . tsubpairs

        tsubpairs ts = [ (hash t, t) | t <- ts ]

        fsub (openSocks, readS) (key, tsub) =
            subscribe
                cfg
                istate
                ( case tsub of
                    (Timer _ _) -> Nothing
                    _ -> Map.lookup key (sockets istate)
                )
                tsub
            >>= ( \(msock, threadinfo) -> return $
                    ( maybe openSocks (\s -> Map.insert key s openSocks) msock
                    , Map.insert key threadinfo readS
                    )
                )


subscribe
    :: Config model msg
    -> InternalState msg
    -> Maybe Socket
    -> TSub msg
    -> IO (Maybe Socket, (SubHandler msg, ThreadId))
subscribe cfg istate msocket (UDP p h) = do
    th <- atomically (newTVar h)
    sock <- getsock
    threadId <- forkIO (runUDPSub cfg istate sock th)
    return (Just sock, (UDPHandler th, threadId))

    where
        getsock = case msocket of
            Just s -> return s
            Nothing -> openUDPPort p

subscribe cfg istate msocket (TCPClient ci getMore h) = do
    th <- atomically (newTVar (getMore, h))
    sock <- getsock
    threadId <- forkIO $ runTCPClientSub cfg istate sock th
    return (Just sock, (TCPClientHandler th, threadId))

    where
        getsock = case msocket of
            Just s -> return s
            Nothing -> connectTCP ci

subscribe cfg istate _ (Timer ms h) = do
    th <- atomically (newTVar h)
    threadId <- forkIO $ runTimerSub cfg istate ms th
    return (Nothing, (TimerHandler th, threadId))



runTCPClientSub
    :: Config model msg
    -> InternalState msg
    -> Socket
    -> TVar (BS.ByteString -> Int, Received -> msg)
    -> IO ()
runTCPClientSub cfg istate sock tfns = forever $ do
    (getMore, th) <- readTVarIO tfns
    bytes <- more sock getMore BS.empty
    now <- getPOSIXTime

    cmd <- atomically $ do
        model <- readTVar tmodel

        let (model2, cmd) = (update cfg) (th (Received bytes now)) model

        writeTVar tmodel model2
        return cmd

    handleCmd cfg istate cmd

    where
        tmodel = fst (init cfg)

        more s f msg =
            let n = f msg in
            if n < 1 then return msg
            else do
                bs <- recv s n
                more s f (msg <> bs)


runUDPSub
    :: Config model msg
    -> InternalState msg
    -> Socket
    -> TVar (CompactInfo -> Received -> msg)
    -> IO ()
runUDPSub cfg istate sock tHandler = forever $ do
    (bs, sockAddr) <- recvFrom sock maxline
    now <- getPOSIXTime

    cmd <- atomically $ do
        model <- readTVar tmodel
        th <- readTVar tHandler

        let (model2, cmd) =
                        (update cfg)
                            (th (addrToCi sockAddr) (Received bs now))
                            model

        writeTVar tmodel model2
        return cmd

    handleCmd cfg istate cmd

    where
        tmodel = fst (init cfg)


runTimerSub
    :: Config model msg
    -> InternalState msg
    -> Int
    -> TVar (POSIXTime -> msg)
    -> IO ()
runTimerSub cfg istate ms tHandler = forever $ do
    threadDelay (1000 * ms)
    now <- getPOSIXTime

    cmd <- atomically $ do
        model <- readTVar tmodel
        th <- readTVar tHandler

        let (model2, cmd) = (update cfg) (th now) model

        writeTVar tmodel model2
        return cmd

    handleCmd cfg istate cmd

    where
        tmodel = fst (init cfg)


handleCmd
    :: Config model msg
    -> InternalState msg
    -> Cmd msg
    -> IO ()
handleCmd cfg istate cmd = do
    writeS <- readTVarIO $ writeThreadS istate

    runCmds
        writeS
        (cmdSink istate)
        cfg { init = (tmodel, cmd) }

    atomically $ do
        model <- readTVar tmodel
        putTMVar (subSink istate) ((subscriptions cfg) model)

    where
        tmodel = fst (init cfg)


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

