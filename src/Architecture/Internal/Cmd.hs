module Architecture.Internal.Cmd
    ( runCmds
    , batch
    , updateWriters
    ) where

import Prelude hiding (init)
import Control.Monad (forever)
import System.Random (randomIO)
import Data.Hashable (hash)
import Data.Maybe (catMaybes)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Crypto.Random (newGenIO, genBytes)
import Crypto.Random.DRBG (CtrDRBG)
import Network.Socket (Socket)
import Network.Socket.ByteString (sendTo, sendAll)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.IO (hFlush, stdout)
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM
    ( atomically
    , TQueue
    , TVar
    , STM
    , newTQueue
    , writeTQueue
    , readTQueue
    , readTVar
    , readTVarIO
    , writeTVar
    , modifyTVar
    )

import Network.KRPC.Types (CompactInfo (CompactInfo))
import Architecture.Internal.Network (openUDPPort, ciToAddr, connectTCP)
import Architecture.Internal.Types
    ( Cmd (..)
    , TCmd (..)
    , TSub (..)
    , CmdQ (..)
    , Config (..)
    , InternalState (..)
    )

execTCmd
    :: Map Int (CmdQ, ThreadId) -- writeThreadS
    -> TQueue (TCmd msg)        -- cmdSink
    -> TCmd msg
    -> IO (Maybe msg)
execTCmd _ _ (CmdGetRandom f) =
    randomIO >>= \i -> return (Just $ f i)

execTCmd _ _(CmdGetTime f) =
    getPOSIXTime >>= \t -> return (Just $ f t)

execTCmd _ _ (CmdLog msg) =
    putStr msg >> hFlush stdout >> return (Nothing)

execTCmd _ _ (CmdRandomBytes n f) =
    do
        g <- newGenIO :: IO CtrDRBG

        case genBytes n g of
            Left err -> error $ show err
            Right (result, _) -> return (Just (f result))


execTCmd _ _ (CmdReadFile filename f) =
    do
        bytes <- BS.readFile filename
        return (Just $ f bytes)


execTCmd _ _ (CmdWriteFile filename bs) =
    do
        BS.writeFile filename bs
        return (Nothing)


execTCmd _ _ (CmdSendUDP _ (CompactInfo ip 0) _) =
    execTCmd undefined undefined $ CmdLog msg

    where
        msg = "Error: Cannot send message to " ++ show (CompactInfo ip 0)
            ++ ". Invalid port 0!"

execTCmd wrT sink cmd = sinkTCmd wrT sink cmd


execCmd
    :: Map Int (CmdQ, ThreadId)
    -> TQueue (TCmd msg)
    -> Cmd msg
    -> IO ([ msg ])
execCmd wrT sink (Cmd l) = (mapM (execTCmd wrT sink) l) >>= (return . catMaybes)


runCmds
    :: Map.Map Int (CmdQ, ThreadId)
    -> TQueue (TCmd msg)
    -> Config model msg
    -> IO ()
runCmds writeS sink cfg = do
    (msgs) <- execCmd writeS sink cmd

    case msgs of
        [] -> return ()
        _ -> do
            cmds <- atomically $ foldMsgsStm (update cfg) msgs tmodel
            runCmds writeS sink (cfg { init = (tmodel, cmds) })

    where
        (tmodel, cmd) = init cfg


updateWriters
    :: TCmd msg
    -> InternalState msg
    -> IO (InternalState msg)
updateWriters (CmdSendUDP srcPort ci bs) istate =
    updateWriters_
        cmd
        istate
        (openUDPPort srcPort)
        newq
        writeUDPMain
        UDPQueue

    where
        newq = do
            q <- newTQueue
            writeTQueue q (ci, bs)
            return q

        cmd = CmdSendUDP srcPort ci bs

updateWriters (CmdSendTCP ci bs) istate =
    updateWriters_
        cmd
        istate
        (connectTCP ci)
        newq
        writeTCPMain
        TCPQueue

    where
        newq = do
            q <- newTQueue
            writeTQueue q bs
            return q

        cmd = CmdSendTCP ci bs

updateWriters _ _ = undefined


updateWriters_
    :: TCmd msg
    -> InternalState msg
    -> IO Socket
    -> STM (TQueue a)
    -> (TQueue a -> Socket -> IO ())
    -> (TQueue a -> CmdQ)
    -> IO (InternalState msg)
updateWriters_ cmd istate getsock getq threadmain initCmdQ = do
    writers <- readTVarIO (writeThreadS istate)

    case Map.lookup key writers of
        (Just (cmdq, _)) -> enqueueCmd cmdq cmd >> return istate
        Nothing -> do
            (sock, istate2) <- case Map.lookup key (sockets istate) of
                (Just sock) -> return (sock, istate)
                Nothing -> do
                    sock <- getsock
                    return
                        ( sock
                        , istate
                            { sockets = Map.insert key sock (sockets istate)
                            }
                        )

            newq <- atomically getq

            threadId <- forkIO (threadmain newq sock)

            atomically $ modifyTVar
                (writeThreadS istate2)
                (Map.insert key (initCmdQ newq, threadId))

            return istate2

    where
        key = getKey cmd


writeUDPMain :: TQueue (CompactInfo, BS.ByteString) -> Socket -> IO ()
writeUDPMain q sock = forever $ do
    (ci, bs) <- atomically $ readTQueue q
    _ <- sendTo sock bs (ciToAddr ci)
    return ()


writeTCPMain :: TQueue BS.ByteString -> Socket -> IO ()
writeTCPMain q sock = forever $ do
    bs <- atomically $ readTQueue q
    sendAll sock bs
    return ()


sinkTCmd
    :: Map Int (CmdQ, ThreadId)
    -> TQueue (TCmd msg)
    -> TCmd msg
    -> IO (Maybe msg)
sinkTCmd wrT sink cmd = do
    case Map.lookup (getKey cmd) wrT of
        (Just (cmdq, _)) -> enqueueCmd cmdq cmd
        Nothing -> atomically $ writeTQueue sink cmd

    return Nothing


enqueueCmd :: CmdQ -> TCmd msg -> IO ()
enqueueCmd (UDPQueue q) (CmdSendUDP _ ci bs) =
    atomically (writeTQueue q (ci, bs))
enqueueCmd (TCPQueue q) (CmdSendTCP _ bs) =
    atomically (writeTQueue q bs)
enqueueCmd _ _ = undefined


foldMsgs
    :: (msg -> model -> (model, Cmd msg))
    -> [ msg ]
    -> model
    -> (model, Cmd msg)
foldMsgs _ [] mdl = (mdl, Cmd [])
foldMsgs f (x:xs) mdl = cmd2 `merge` foldMsgs f xs mdl2
    where
        (mdl2, cmd2) = f x mdl


foldMsgsStm
    :: (msg -> model -> (model, Cmd msg))
    -> [ msg ]
    -> TVar model
    -> STM (Cmd msg)
foldMsgsStm up msgs tmodel = do
    model <- readTVar tmodel
    let (model2, cmds) = foldMsgs up msgs model
    writeTVar tmodel model2
    return cmds


merge :: Cmd msg -> (model, Cmd msg) -> (model, Cmd msg)
merge c1 (m, c2) = (m, batch [c1, c2])


batch :: [ Cmd msg ] -> Cmd msg
batch cmds = Cmd $ concat [t | (Cmd t) <- cmds]


getKey :: TCmd msg -> Int
getKey (CmdSendUDP srcPort _ _) = hash $ UDP srcPort undefined
getKey (CmdSendTCP ci _)        = hash $ TCPClient ci undefined undefined
getKey _                        = undefined
