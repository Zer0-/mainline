{-# LANGUAGE NamedFieldPuns, LambdaCase #-}

module Architecture.Internal.Sub
    ( Received (..)
    , updateSubscriptions
    , connectTCP
    , ciToAddr
    , mapTSub
    , hashSub
    ) where

import Prelude hiding (init)
import Data.List (foldl')
import Control.Monad (foldM)
import Data.Foldable (sequence_)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import Network.Socket (Socket, close, SockAddr)
import Network.Socket.ByteString (recv, recvFrom)
--import System.Timeout (timeout)
import Data.Hashable (hash, hashWithSalt)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Control.Concurrent (forkIO, ThreadId, threadDelay, killThread)
import Control.Concurrent.STM
    ( STM
    , TVar
    , newTVar
    , atomically
    , readTVarIO
    , readTVar
    , writeTVar
    )
import Control.Concurrent.MVar
    ( MVar
    , takeMVar
    , putMVar
    , newEmptyMVar
    )
import Control.Exception.Safe (catchIO, onException)

import Network.KRPC.Types (CompactInfo)
import Architecture.Internal.Types
    ( InternalState (..)
    , Program (..)
    , Received (..)
    , TSub (..)
    , SubHandler (..)
    , Sub (..)
    , SocketMood (..)
    )
import Architecture.Internal.Cmd (updateOnFailure, handleCmd)
import Architecture.Internal.Network
    ( openUDPPort
    , connectTCP
    , ciToAddr
    , addrToCi
    , openSocket
    )

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
    :: Sub msg
    -> Program model msg schemas
    -> InternalState msg schemas
    -> IO (InternalState msg schemas)
updateSubscriptions (Sub tsubs) cfg istate = do
    writeS <- atomically $ do
        updateHandlers currentReads tsubpairs

        ws <- readTVar (writeThreadS istate)

        sequence_ $ map
            ((flip writeTVar) True)
            ( map
                (\(_, x, _) -> x)
                (Map.elems (ws `Map.restrictKeys` (Map.keysSet unloads)))
            )

        return ws
    
    (newsocks, loaded) <- foldM fsub (Map.empty, Map.empty) loads

    mapM_ killThread (Map.map snd unloads)

    let
        toClose = Set.filter
            ((\case
                WantWrites _  -> False
                WantReads _ _ -> True
                WantBoth _    -> False
                HaveSocket _  -> True
            ) . ((Map.!) socks))
            (fullClosed writeS)

    -- should the SocketMood WantBoth be changed to WantWrites if it's in fullClosed?

    mapM_ closeSocketMood (socks `Map.restrictKeys` toClose)

    return istate
        { readThreadS =
            Map.union loaded (currentReads `Map.difference` unloads)
        , sockets =
            newsocks `Map.union` (socks `Map.withoutKeys` toClose)
        , curSubHash = hashSub (Sub tsubs)
        }

    where
        loads =
            ( filter (\(k, _) -> Map.notMember k currentReads)
            ) tsubpairs

        socks = sockets istate

        unloads =
            currentReads `Map.withoutKeys` (Set.fromList (map fst tsubpairs))

        fullClosed writeS = Set.difference
            (Map.keysSet unloads)
            (Map.keysSet writeS)

        tsubpairs = [ (hash t, t) | t <- tsubs ]

        currentReads = readThreadS istate

        fsub (openSocks, readS) (key, tsub) =
            subscribe
                cfg
                istate
                key
                ( case tsub of
                    (Timer _ _) -> Nothing
                    _ -> Map.lookup key socks
                )
                tsub
            >>=
                return . ( \(msocketm, mread) -> do
                    let newSocks s = Map.insert key s openSocks
                    let newReadS r = Map.insert key r readS

                    case msocketm of
                        Just sm ->
                            case mread of
                                Just r -> (newSocks sm, newReadS r)
                                Nothing -> (newSocks sm, readS)
                        Nothing ->
                            case mread of
                                Just r -> (openSocks, newReadS r)
                                Nothing -> (openSocks, readS)
                )



updateHandlers
    :: Map.Map Int (SubHandler msg, ThreadId)
    -> [ (Int, TSub msg) ]
    -> STM ()
updateHandlers rs tsubs =
    sequence_ [ updateH key s tsubsMap | (key, (s, _)) <- Map.toList rs ]

    where
        tsubsMap = Map.fromList tsubs

        updateH :: Int -> SubHandler msg -> Map.Map Int (TSub msg) -> STM ()
        updateH key subh ts =
            maybe
            (return ())
            (writeHdlr subh)
            (Map.lookup key ts)


writeHdlr :: SubHandler msg -> TSub msg -> STM ()
writeHdlr (TCPClientHandler tv) (TCPClient _ _ _ getMore h _) =
    writeTVar tv (getMore, h)
writeHdlr (UDPHandler tv) (UDP _ h _) = writeTVar tv h
writeHdlr (TimerHandler tv) (Timer _ h) = writeTVar tv h
writeHdlr _ _ = undefined


updateOwnHandler :: Int -> (SubHandler msg) -> Sub msg -> STM ()
updateOwnHandler key subHdlr (Sub tsubs) =
    case toupdate of
        [] -> return ()
        (x:_) -> writeHdlr subHdlr (snd x)

    where
        toupdate = filter ((== key) . fst) tsubpairs
        tsubpairs = [ (hash t, t) | t <- tsubs ]


subscribe
    :: Program model msg schemas
    -> InternalState msg schemas
    -> Int
    -> Maybe (SocketMood msg schemas)
    -> TSub msg
    -> IO (Maybe (SocketMood msg schemas), Maybe (SubHandler msg, ThreadId))
subscribe cfg istate key msocket (UDP p h failmsg) = do
    th <- atomically (newTVar h)
    sock <- getsock
    threadId <- forkIO $
        killableProducerConsumer
            (udpProduce sock)
            (udpConsume cfg istate key th failmsg)
    return (Just $ HaveSocket sock, Just (UDPHandler th, threadId))

    where
        getsock = case msocket of
            Just (HaveSocket s) -> return s
            Just _ -> error "udp sockets expected to open synchronously"
            Nothing -> openUDPPort p

subscribe cfg istate key msocket (TCPClient t ci fkey getMore h failmsg) =
    case msocket of
        Just (HaveSocket sock) -> do
            th <- atomically (newTVar (getMore, h))
            threadId <- forkIO $
                killableProducerConsumer
                    (tcpClientProduce th sock)
                    (tcpClientConsume cfg istate th key failmsg)
            return (Just $ HaveSocket sock, Just (TCPClientHandler th, threadId))

        Just (WantWrites tcmdq) ->
            return (Just $ WantBoth (tcmdq, tsub), Nothing)

        Just (WantReads threadid _) ->
            return (Just $ WantReads threadid tsub, Nothing)

        Just (WantBoth (tcmdq, _)) ->
            return (Just $ WantBoth (tcmdq, tsub), Nothing)

        Nothing -> do
            threadid <- forkIO $ openSocket
                key
                (connectTCP ci)
                (cmdSink istate)
                onFail

            -- do we need the TSub in SocketMood state? We do need the
            -- list of queued up Cmds.

            return (Just $ WantReads threadid tsub, Nothing)

    where
        tsub = TCPClient t ci fkey getMore h failmsg
        onFail = updateOnFailure istate cfg failmsg


subscribe cfg istate key _ (Timer ms h) = do
    th <- atomically (newTVar h)
    threadId <- forkIO $
        killableProducerConsumer
            (timerProduce ms)
            (timerConsume cfg istate key th)
    return $ (Nothing, Just $ (TimerHandler th, threadId))


tcpClientProduce
    :: TVar (BS.ByteString -> Int, Received -> msg)
    -> Socket
    -> IO (Maybe BS.ByteString)
tcpClientProduce tfns sock = do
    (getMore, _) <- readTVarIO tfns

    catchIO
        (more sock getMore BS.empty)
        (\_ -> return Nothing)

    where
        more s f msg =
            let n = f msg in
            if n < 1 then return (Just msg)
            else do
                bs <- recv s (min 4096 n)
                if BS.length bs == 0 then return Nothing
                else more s f (msg <> bs)


tcpClientConsume
    :: Program model msg schemas
    -> InternalState msg schemas
    -> TVar (BS.ByteString -> Int, Received -> msg)
    -> Int
    -> msg
    -> (Maybe BS.ByteString)
    -> IO ()
tcpClientConsume cfg istate _ _ failmsg Nothing =
    updateOnFailure istate cfg failmsg

tcpClientConsume cfg istate tfns key _ (Just bytes) = do
    now <- getPOSIXTime
    (_, th) <- readTVarIO tfns

    cmd <- atomically $ do
        model <- readTVar tmodel
        let (model2, cmd) = (update cfg) (th (Received bytes now)) model

        writeTVar tmodel model2

        updateOwnHandler
            key
            (TCPClientHandler tfns)
            ((subscriptions cfg) model2)

        return cmd

    handleCmd cfg istate cmd

    where
        tmodel = fst (init cfg)

udpProduce :: Socket -> IO (Maybe (BS.ByteString, SockAddr))
udpProduce sock =
    catchIO
        (recvFrom sock maxline >>= return . Just)
        (\_ -> return Nothing)

udpConsume
    :: Program model msg schemas
    -> InternalState msg schemas
    -> Int
    -> TVar (CompactInfo -> Received -> msg)
    -> msg
    -> Maybe (BS.ByteString, SockAddr)
    -> IO ()
udpConsume cfg istate _ _ failmsg Nothing = updateOnFailure istate cfg failmsg
udpConsume cfg istate key tth _ (Just (bs, sockAddr)) = do
    now <- getPOSIXTime

    cmd <- atomically $ do
        model <- readTVar tmodel
        th <- readTVar tth

        let (model2, cmd) =
                        (update cfg)
                            (th (addrToCi sockAddr) (Received bs now))
                            model

        writeTVar tmodel model2

        updateOwnHandler key (UDPHandler tth) ((subscriptions cfg) model2)

        return cmd

    handleCmd cfg istate cmd

    where
        tmodel = fst (init cfg)

timerProduce :: Int -> IO (Maybe POSIXTime)
timerProduce ms = threadDelay (1000 * ms) >> getPOSIXTime >>= return . Just

timerConsume
    :: Program model msg schemas
    -> InternalState msg schemas
    -> Int -- key
    -> TVar (POSIXTime -> msg)
    -> Maybe (POSIXTime)
    -> IO ()
timerConsume _ _ _ _ Nothing = undefined
timerConsume cfg istate key tth (Just now) = do
    cmd <- atomically $ do
        model <- readTVar tmodel
        th <- readTVar tth

        let (model2, cmd) = (update cfg) (th now) model

        writeTVar tmodel model2

        updateOwnHandler key (TimerHandler tth) ((subscriptions cfg) model2)

        return cmd

    handleCmd cfg istate cmd

    where
        tmodel = fst (init cfg)


closeSocketMood :: SocketMood msg schemas -> IO ()
closeSocketMood (HaveSocket sock) = close sock
closeSocketMood (WantWrites _) = return ()
closeSocketMood (WantReads t _) = killThread t
closeSocketMood (WantBoth _) = return ()


mapTSub :: (msg0 -> msg1) -> TSub msg0 -> TSub msg1
mapTSub f (TCPClient t ci fkey g h e) = TCPClient t ci fkey g (f . h) (f e)
mapTSub f (UDP p h e)                 = UDP p (\ci -> f . (h ci)) (f e)
mapTSub f (Timer ms h)                = Timer ms (f . h)


-- For optimization only
hashSub :: Sub msg -> Int
hashSub (Sub ss) = reduceHash ss
    where
        reduceHash :: [ TSub msg ] -> Int
        reduceHash = foldl' hash_ (0 :: Int)

        hash_ s (TCPClient t ci fkey more h msg) = s
            `hashWithSalt` (TCPClient t ci fkey more h msg)
            `hashWithSalt` fkey

        hash_ s sub = hashWithSalt s sub


killableProducerConsumer :: IO (Maybe a) -> (Maybe a -> IO ()) -> IO ()
killableProducerConsumer ioproduce ioconsume = do
    msgs <- newEmptyMVar
    _ <- forkIO $ consumer ioconsume msgs

    let loop = do
            mval <- ioproduce
            putMVar msgs (Just mval)
            takeMVar msgs >> loop

    loop `onException` putMVar msgs Nothing


consumer :: (Maybe a -> IO ()) -> MVar (Maybe (Maybe a)) -> IO ()
consumer consume mvar = do
    mx <- takeMVar mvar
    case mx of
        Nothing -> return ()
        Just x -> consume x >> putMVar mvar Nothing >> consumer consume mvar
