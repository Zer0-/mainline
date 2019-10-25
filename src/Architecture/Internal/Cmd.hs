{-# LANGUAGE
    DataKinds
  , KindSignatures
  , ExistentialQuantification
#-}

module Architecture.Internal.Cmd
    ( runCmds
    , batch
    , updateWriters
    , mapTCmd
    , foldMsgsStm
    ) where

import Prelude hiding (init)
import Control.Exception.Safe (catchIO)
import System.Random (randomIO)
import Data.Hashable (hash)
import Data.Maybe (catMaybes, fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Crypto.Random (newGenIO, genBytes)
import Crypto.Random.DRBG (CtrDRBG)
import Network.Socket (Socket, close)
import Network.Socket.ByteString (sendTo, sendAll)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.IO (hFlush, stdout)
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM
    ( atomically
    , orElse
    , retry
    , TQueue
    , TVar
    , STM
    , newTVar
    , newTQueue
    , writeTQueue
    , readTQueue
    , readTVar
    , readTVarIO
    , writeTVar
    , modifyTVar
    )
import Squeal.PostgreSQL (Connection, Pool, usingConnectionPool)
import Generics.SOP (K (..))

import Network.KRPC.Types (CompactInfo (CompactInfo))
import Architecture.Internal.Network (openUDPPort, ciToAddr, connectTCP)
import Architecture.Internal.Types
    ( Cmd (..)
    , TCmd (..)
    , TSub (..)
    , CmdQ (..)
    , Program (..)
    , InternalState (..)
    )

import Debug.Trace (trace)

execTCmd
    :: TVar (Map Int (CmdQ, a, ThreadId))  -- writeThreadS
    -> TQueue (TCmd msg schemas)           -- cmdSink
    -> Maybe (Pool (K Connection schemas)) -- db pool
    -> TCmd msg schemas
    -> IO (Maybe msg)
execTCmd _ _ _ (CmdGetRandom f) =
    randomIO >>= \i -> return (Just $ f i)

execTCmd _ _ _ (CmdGetTime f) =
    getPOSIXTime >>= \t -> return (Just $ f t)

execTCmd _ _ _ (CmdLog msg) =
    putStr msg >> hFlush stdout >> return (Nothing)

execTCmd _ _ _ (CmdRandomBytes n f) =
    do
        g <- newGenIO :: IO CtrDRBG

        case genBytes n g of
            Left err -> error $ show err
            Right (result, _) -> return (Just (f result))


execTCmd _ _ _ (CmdReadFile filename f) =
    do
        bytes <- BS.readFile filename
        return (Just $ f bytes)


execTCmd _ _ _ (CmdWriteFile filename bs) =
    do
        BS.writeFile filename bs
        return (Nothing)


execTCmd _ _ _ (CmdSendUDP _ (CompactInfo ip 0) _ failmsg) = do
    _ <- execTCmd undefined undefined undefined $ CmdLog msg
    return $ Just failmsg

    where
        msg = "Error: Cannot send message to " ++ show (CompactInfo ip 0)
            ++ ". Invalid port 0!"

execTCmd _ _ (Just pool) (CmdDatabase session (Just handler)) =
    usingConnectionPool pool $ session >>= return . Just . handler

execTCmd _ _ (Just pool) (CmdDatabase session Nothing) =
    usingConnectionPool pool $ session >> return Nothing

execTCmd _ _ Nothing (CmdDatabase _ _) = undefined

execTCmd _ _ _ (CmdBounce m) = return (Just m)

execTCmd wrT sink _ (CmdSendTCP t ci bs msg) = do
    putStrLn $ "execTCmd CmdSendTCP to " ++ show ci
    atomically $ sinkTCmd wrT sink (CmdSendTCP t ci bs msg) >> return Nothing

execTCmd wrT sink _ cmd = atomically $ sinkTCmd wrT sink cmd >> return Nothing


execCmd
    :: TVar (Map Int (CmdQ, a, ThreadId))
    -> TQueue (TCmd msg schemas)
    -> Maybe (Pool (K Connection schemas))
    -> Cmd msg schemas
    -> IO ([ msg ])
execCmd wrT sink mpool (Cmd l) =
    (mapM (execTCmd wrT sink mpool) l) >>= (return . catMaybes)


runCmds
    :: InternalState msg schemas
    -> Program model msg schemas
    -> IO ()
runCmds istate cfg = do
    msgs <- execCmd (writeThreadS istate)
                    (cmdSink istate)
                    (dbPool istate)
                    cmd

    case msgs of
        [] -> return ()
        _ -> do
            cmds <- atomically $ foldMsgsStm (update cfg) msgs tmodel
            runCmds istate (cfg { init = (tmodel, cmds) })

    where
        (tmodel, cmd) = init cfg


updateWriters
    :: TCmd msg schemas
    -> InternalState msg schemas
    -> Program model msg schemas
    -> IO (InternalState msg schemas)
updateWriters (CmdSendUDP srcPort ci bs failmsg) istate cfg =
    updateWriters_
        cmd
        istate
        cfg
        (openUDPPort srcPort)
        failmsg
        newq
        writeUDPMain
        UDPQueue

    where
        newq = do
            q <- newTQueue
            writeTQueue q (ci, bs)
            return q

        cmd = CmdSendUDP srcPort ci bs failmsg

updateWriters (CmdSendTCP t ci bs failmsg) istate cfg =
    updateWriters_
        cmd
        istate
        cfg
        (connectTCP ci)
        failmsg
        newq
        writeTCPMain
        TCPQueue

    where
        newq = do
            q <- newTQueue
            writeTQueue q bs
            return q

        cmd = CmdSendTCP t ci bs failmsg

updateWriters (QuitW key) istate _ = do
    putStrLn "Quitting"
    atomically $ modifyTVar
        (writeThreadS istate)
        (Map.delete key)

    if Map.member key (readThreadS istate)
    then return istate
    else maybe
        (return istate)
        (\socket ->
            close socket
            >> return istate { sockets = Map.delete key (sockets istate) }
        )
        (Map.lookup key (sockets istate))

updateWriters _ _ _ = undefined


updateWriters_
    :: TCmd msg schemas
    -> InternalState msg schemas
    -> Program model msg schemas
    -> IO Socket
    -> msg
    -> STM (TQueue a)
    ->
        ( TQueue a
        -> TVar Bool
        -> Int
        -> TQueue (TCmd msg schemas)
        -> Socket
        -> IO ()
        )
    -> (TQueue a -> CmdQ)
    -> IO (InternalState msg schemas)
updateWriters_ cmd istate cfg getsock failmsg getq threadmain initCmdQ = do
    writers <- readTVarIO (writeThreadS istate)

    case Map.lookup key writers of
        (Just (cmdq, _, _)) -> atomically $ enqueueCmd cmdq cmd >> return istate

        Nothing -> do
            (msock, istate2) <- case Map.lookup key (sockets istate) of
                (Just sock) -> return (Just sock, istate)

                Nothing -> do
                    putStrLn "Opening new socket" 

                    sock <- catchIO
                                (getsock >>= return . Just)
                                (trace "t failed getting socket" (\_ -> return Nothing))
                    return
                        ( sock
                        , istate
                            { sockets =
                                Map.insert key (fromJust sock) (sockets istate)
                            }
                        )

            case msock of
                Nothing -> do
                    putStrLn "updateWriters_ failed to get a new socket (writing)"
                    cmds <- atomically $ foldMsgsStm (update cfg) [failmsg] tmodel
                    putStrLn "updateWriters_ have cmds for failmsg"
                    runCmds istate cfg { init = (fst (init cfg), cmds) }
                    putStrLn "updateWriters_ runCmds done, returning"
                    return istate
                Just (sock) -> do
                    putStrLn "have just msock (new writer socket)"

                    (newq, tquit) <- atomically $ do
                        tquit <- newTVar quit
                        newq <- getq

                        return (newq, tquit)

                    putStrLn "forking new writer thread"

                    threadId <- forkIO
                                    ( threadmain
                                        newq
                                        tquit
                                        key
                                        (cmdSink istate)
                                        sock
                                    )

                    atomically $ modifyTVar
                        (writeThreadS istate2)
                        (Map.insert key (initCmdQ newq, tquit, threadId))

                    putStrLn "updateWriters returning"

                    return istate2

    where
        quit = Map.null (readThreadS istate)
        key = getKey cmd
        tmodel = fst $ init cfg


writeUDPMain
    :: TQueue (CompactInfo, BS.ByteString)
    -> TVar Bool
    -> Int
    -> TQueue (TCmd msg schemas)
    -> Socket
    -> IO ()
writeUDPMain q quit key qsink sock = runMain q quit key qsink sock send
    where
        send (ci, bs) = trace ("t sending udp " ++ show key) $ sendTo sock bs (ciToAddr ci) >> return ()


writeTCPMain
    :: TQueue BS.ByteString
    -> TVar Bool
    -> Int
    -> TQueue (TCmd msg schemas)
    -> Socket
    -> IO ()
writeTCPMain q quit key qsink sock = runMain q quit key qsink sock send
    where
        send = trace ("t sending tcp " ++ show key) $ sendAll sock


runMain
    :: TQueue a
    -> TVar Bool
    -> Int
    -> TQueue (TCmd msg schemas)
    -> Socket
    -> (a -> IO ())
    -> IO ()
runMain q quit key qsink sock fsend = do
    -- STUCK HERE!! - of course it's stuck here,
    -- there are no sub threads.
    putStrLn $ show key ++ " runMain - top"
    msend <- atomically $ lexpr `orElse` rexpr

    case msend of
        Just x -> do
            putStrLn $ show key ++ " runMain - Sending on socket"
            fsend x
            runMain q quit key qsink sock fsend
        Nothing -> do
            putStrLn $ show key ++ " runMain - Quit condition reached, writing qsink"
            atomically $ writeTQueue qsink (QuitW key)
            return ()

    where
        lexpr = readTQueue q >>= return . Just
        rexpr = readTVar quit >>= \q_ -> if q_ then (return Nothing) else retry


sinkTCmd
    :: TVar (Map Int (CmdQ, a, ThreadId))
    -> TQueue (TCmd msg schemas)
    -> TCmd msg schemas
    -> STM ()
sinkTCmd writeS sink cmd = do
    wrT <- readTVar writeS

    case Map.lookup (getKey cmd) wrT of
        (Just (cmdq, _, _)) -> trace "t sinkTCmd - have a appropriate write thread, passing cmd." $ enqueueCmd cmdq cmd
        Nothing -> trace "t sinkTCmd do not have write thread for this cmd, sinking to main thread" $ writeTQueue sink cmd


enqueueCmd :: CmdQ -> TCmd msg schemas -> STM ()
enqueueCmd (UDPQueue q) (CmdSendUDP _ ci bs _) = writeTQueue q (ci, bs)
enqueueCmd (TCPQueue q) (CmdSendTCP _ _  bs _) = writeTQueue q bs
enqueueCmd _            _                      = undefined


foldMsgs
    :: (msg -> model -> (model, Cmd msg schemas))
    -> [ msg ]
    -> model
    -> (model, Cmd msg schemas)
foldMsgs _ [] mdl = (mdl, Cmd [])
foldMsgs f (x:xs) mdl = cmd2 `merge` foldMsgs f xs mdl2
    where
        (mdl2, cmd2) = f x mdl


foldMsgsStm
    :: (msg -> model -> (model, Cmd msg schemas))
    -> [ msg ]
    -> TVar model
    -> STM (Cmd msg schemas)
foldMsgsStm up msgs tmodel = do
    model <- readTVar tmodel
    let (model2, cmds) = foldMsgs up msgs model
    writeTVar tmodel model2
    return cmds


merge :: Cmd msg schemas -> (model, Cmd msg schemas) -> (model, Cmd msg schemas)
merge c1 (m, c2) = (m, batch [c1, c2])


batch :: [ Cmd msg schemas ] -> Cmd msg schemas
batch cmds = Cmd $ concat [t | (Cmd t) <- cmds]


getKey :: TCmd msg schemas -> Int
getKey (CmdSendUDP srcPort _ _ _) = hash $ UDP srcPort undefined
getKey (CmdSendTCP t ci _ _)      = hash $ TCPClient t ci undefined undefined undefined
getKey _                          = undefined


mapTCmd :: (msg0 -> msg1) -> TCmd msg0 schemas -> TCmd msg1 schemas
mapTCmd _ (CmdLog a)                  = CmdLog a
mapTCmd f (CmdGetRandom h)            = CmdGetRandom (f . h)
mapTCmd f (CmdGetTime h)              = CmdGetTime (f . h)
mapTCmd f (CmdRandomBytes n h)        = CmdRandomBytes n (f . h)
mapTCmd f (CmdSendUDP p ci bs err)    = CmdSendUDP p ci bs (f err)
mapTCmd f (CmdSendTCP t ci bs err)    = CmdSendTCP t ci bs (f err)
mapTCmd f (CmdReadFile p h)           = CmdReadFile p (f . h)
mapTCmd _ (CmdWriteFile p bs)         = CmdWriteFile p bs
mapTCmd f (CmdDatabase sesh (Just h)) = CmdDatabase sesh (Just $ f . h)
mapTCmd _ (CmdDatabase sesh Nothing)  = CmdDatabase sesh Nothing
mapTCmd f (CmdBounce m)               = CmdBounce (f m)
mapTCmd _ (QuitW i)                   = QuitW i
