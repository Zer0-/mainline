module Architecture.Internal.Cmd
    ( runCmds
    , batch
    ) where

import Prelude hiding (init)
import System.Random (randomIO)
import Data.Hashable (hash)
import Data.Maybe (catMaybes)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Crypto.Random (newGenIO, genBytes)
import Crypto.Random.DRBG (CtrDRBG)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.IO (hFlush, stdout)
import Control.Concurrent (ThreadId)
import Control.Concurrent.STM
    ( atomically
    , TQueue
    , TVar
    , STM
    , writeTQueue
    , readTVar
    , writeTVar
    )

import Network.KRPC.Types (CompactInfo (CompactInfo))
import Architecture.Internal.Types
    ( Cmd (..)
    , TCmd (..)
    , TSub (..)
    , CmdQ (..)
    , Config (..)
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

execTCmd wrT sink (CmdSendUDP srcPort dest bs) = sendNetTCmd wrT sink cmd key
    where
        key = hash (UDP srcPort undefined)
        cmd = CmdSendUDP srcPort dest bs

execTCmd wrT sink (CmdSendTCP ci bs) = sendNetTCmd wrT sink cmd key
    where
        key = hash (TCPClient ci undefined undefined)
        cmd = (CmdSendTCP ci bs)


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
       _ -> atomically (foldMsgsStm (update cfg) msgs tmodel) >>=
           \cmds -> runCmds writeS sink $ cfg { init = (tmodel, cmds) }

    where
        (tmodel, cmd) = init cfg


sendNetTCmd
    :: Map Int (CmdQ, ThreadId)
    -> TQueue (TCmd msg)
    -> TCmd msg
    -> Int
    -> IO (Maybe msg)
sendNetTCmd wrT sink cmd key = do
    case Map.lookup key wrT of
        (Just (cmdq, _)) -> enqueueCmd cmdq cmd
        Nothing -> atomically $ writeTQueue sink cmd

    return Nothing


enqueueCmd :: CmdQ -> TCmd msg -> IO ()
enqueueCmd (UDPQueue q) (CmdSendUDP p ci bs) =
    atomically (writeTQueue q (p, ci, bs))

enqueueCmd (TCPQueue q) (CmdSendTCP ci bs) =
    atomically (writeTQueue q (ci, bs))

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
