module Architecture.Internal.Cmd
    ( execCmd
    ) where

import System.Random (randomIO)
import Control.Monad (foldM)
import Data.Hashable (hash)
import Data.Maybe (catMaybes)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Crypto.Random (newGenIO, genBytes)
import Crypto.Random.DRBG (CtrDRBG)
import Network.Socket.ByteString (sendTo, sendAll)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.IO (hFlush, stdout)
import Control.Concurrent (ThreadId)
import Control.Concurrent.STM (atomically, TQueue, writeTQueue)

import Network.KRPC.Types (CompactInfo (CompactInfo))
import Architecture.Internal.Types
    ( InternalState (..)
    , Cmd (..)
    , TCmd (..)
    , TSub (..)
    , CmdQ (..)
    )
import Architecture.Internal.Sub
    ( openUDPPort
    , connectTCP
    , ciToAddr
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
