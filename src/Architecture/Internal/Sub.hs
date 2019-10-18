{-# LANGUAGE NamedFieldPuns #-}

module Architecture.Internal.Sub
    ( Received (..)
    , updateSubscriptions
    , connectTCP
    , ciToAddr
    , mapTSub
    ) where

import Prelude hiding (init)
import Control.Exception.Safe (catchIO)
import Control.Monad (foldM, forever)
import Data.Foldable (sequence_)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import Network.Socket (Socket, close)
import Network.Socket.ByteString (recv, recvFrom)
--import System.Timeout (timeout)
import Data.Hashable (hash)
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
    , putTMVar
    )

import Network.KRPC.Types (CompactInfo)
import Architecture.Internal.Types
    ( InternalState (..)
    , Program (..)
    , Received (..)
    , TSub (..)
    , Cmd (..)
    , SubHandler (..)
    , Sub (..)
    )
import Architecture.Internal.Cmd (runCmds, foldMsgsStm)
import Architecture.Internal.Network
    ( openUDPPort
    , connectTCP
    , ciToAddr
    , addrToCi
    )

import Debug.Trace (trace)

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
            (map (\(_, x, _) -> x) (Map.elems (ws `Map.restrictKeys` (Map.keysSet unloads))))

        return ws

    (newsocks, loaded) <- foldM fsub (Map.empty, Map.empty) loads

    mapM_ killThread (Map.map snd unloads)

    let toClose = fullClosed writeS

    mapM_ close ((sockets istate) `Map.restrictKeys` toClose)

    return istate
        { readThreadS = Map.union loaded (currentReads `Map.difference` unloads)
        , sockets =
            newsocks `Map.union` ((sockets istate) `Map.withoutKeys` toClose)
        }

    where
        loads =
            ( filter (\(k, _) -> Map.notMember k currentReads)
            ) tsubpairs

        unloads =
            currentReads `Map.withoutKeys` (Set.fromList (map fst tsubpairs))

        fullClosed writeS = (Map.keysSet unloads) `Set.difference` (Map.keysSet writeS)

        tsubpairs = [ (hash t, t) | t <- tsubs ]

        currentReads = readThreadS istate

        fsub (openSocks, readS) (key, tsub) =
            subscribe
                cfg
                istate
                key
                ( case tsub of
                    (Timer _ _) -> Nothing
                    _ -> Map.lookup key (sockets istate)
                )
                tsub
            >>= maybe
                    (return (openSocks, readS))
                    ( \(msock, threadinfo) -> return $
                        ( maybe
                            openSocks
                            (\s -> Map.insert key s openSocks)
                            msock
                        , Map.insert key threadinfo readS
                        )
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
writeHdlr (TCPClientHandler tv) (TCPClient _ _ getMore h _) =
    writeTVar tv (getMore, h)
writeHdlr (UDPHandler tv) (UDP _ h) = writeTVar tv h
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
    -> Maybe Socket
    -> TSub msg
    -> IO (Maybe (Maybe Socket, (SubHandler msg, ThreadId)))
subscribe cfg istate key msocket (UDP p h) = do
    th <- atomically (newTVar h)
    sock <- getsock
    threadId <- forkIO (runUDPSub cfg istate key sock th)
    return $ Just (Just sock, (UDPHandler th, threadId))

    where
        getsock = case msocket of
            Just s -> return s
            Nothing -> openUDPPort p

subscribe cfg istate key msocket (TCPClient _ ci getMore h failmsg) = do
    msock <- getsock

    case msock of
        Nothing -> do
            putStrLn "in subscribe: opening new TCP socket failed"
            cmds <- atomically $ foldMsgsStm (update cfg) [failmsg] tmodel
            runCmds istate (cfg { init = (tmodel, cmds) })
            return Nothing
        Just sock -> do
            putStrLn "in subscribe: successfully opened new TCP socket"
            th <- atomically (newTVar (getMore, h))
            threadId <- forkIO $ runTCPClientSub cfg istate key sock th failmsg
            return $ Just (Just sock, (TCPClientHandler th, threadId))

    where
        getsock = case msocket of
            Just s -> return $ Just s
            Nothing -> catchIO
                (connectTCP ci >>= return . Just)
                (\_ -> return Nothing)

        tmodel = fst $ init cfg

subscribe cfg istate key _ (Timer ms h) = do
    th <- atomically (newTVar h)
    threadId <- forkIO $ runTimerSub cfg istate key ms th
    return $ Just (Nothing, (TimerHandler th, threadId))



runTCPClientSub
    :: Program model msg schemas
    -> InternalState msg schemas
    -> Int
    -> Socket
    -> TVar (BS.ByteString -> Int, Received -> msg)
    -> msg
    -> IO ()
runTCPClientSub cfg istate key sock tfns failmsg = do
    (getMore, th) <- readTVarIO tfns
    mbytes <- catchIO
        ((more sock getMore BS.empty) >>= return . Just)
        (\_ -> return Nothing)

    case mbytes of
        Nothing -> do
            putStrLn "TCP recv failed"
            cmds <- atomically $ foldMsgsStm (update cfg) [failmsg] tmodel
            runCmds istate (cfg { init = (tmodel, cmds) })
        Just bytes -> do
            putStrLn "TCP read OK, have bytes."
            now <- getPOSIXTime

            cmd <- atomically $ do
                model <- readTVar tmodel

                let (model2, cmd) = (update cfg) (th (Received bytes now)) model

                writeTVar tmodel model2

                updateOwnHandler key (TCPClientHandler tfns) ((subscriptions cfg) model2)

                return cmd

            handleCmd cfg istate cmd
            runTCPClientSub cfg istate key sock tfns failmsg

    where
        tmodel = fst (init cfg)

        more s f msg =
            let n = f msg in
            if n < 1 then return msg
            else do
                bs <- recv s n
                more s f (msg <> bs)


runUDPSub
    :: Program model msg schemas
    -> InternalState msg schemas
    -> Int
    -> Socket
    -> TVar (CompactInfo -> Received -> msg)
    -> IO ()
runUDPSub cfg istate key sock tHandler = forever $ do
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

        updateOwnHandler key (UDPHandler tHandler) ((subscriptions cfg) model2)

        return cmd

    handleCmd cfg istate cmd

    where
        tmodel = fst (init cfg)


runTimerSub
    :: Program model msg schemas
    -> InternalState msg schemas
    -> Int -- key
    -> Int -- timeout (ms)
    -> TVar (POSIXTime -> msg)
    -> IO ()
runTimerSub cfg istate key ms tHandler = forever $ do
    threadDelay (1000 * ms)
    now <- getPOSIXTime

    cmd <- atomically $ do
        model <- readTVar tmodel
        th <- readTVar tHandler

        let (model2, cmd) = (update cfg) (th now) model

        writeTVar tmodel model2

        updateOwnHandler key (TimerHandler tHandler) ((subscriptions cfg) model2)

        return cmd

    handleCmd cfg istate cmd

    where
        tmodel = fst (init cfg)


handleCmd
    :: Program model msg schemas
    -> InternalState msg schemas
    -> Cmd msg schemas
    -> IO ()
handleCmd cfg istate cmd = do
    runCmds istate cfg { init = (tmodel, cmd) }

    atomically $ do
        model <- readTVar tmodel
        putTMVar (subSink istate) ((subscriptions cfg) model)

    where
        tmodel = fst (init cfg)

mapTSub :: (msg0 -> msg1) -> TSub msg0 -> TSub msg1
mapTSub f (TCPClient t ci g h e) = TCPClient t ci g (f . h) (f e)
mapTSub f (UDP p h)              = UDP p (\ci -> f . (h ci))
mapTSub f (Timer ms h)           = Timer ms (f . h)
