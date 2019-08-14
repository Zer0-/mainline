{-# LANGUAGE NamedFieldPuns #-}

module Architecture.TEA
    ( Config (Config)
    , run
    ) where

import Prelude hiding (init)
import qualified Data.Map as Map
import Control.Concurrent (ThreadId)
import Control.Concurrent.STM
    ( STM
    , TVar
    , TQueue
    , newTVar
    , newEmptyTMVar
    , newTQueue
    , readTVar
    , readTVarIO
    , writeTVar
    , atomically
    )

import Architecture.Internal.Cmd (execCmd)
import Architecture.Internal.Types
    ( InternalState (..)
    , Cmd (..)
    , Sub (..)
    )
import qualified Architecture.Internal.Types as T
import Architecture.Internal.Sub (updateSubscriptions)
import qualified Architecture.Cmd as Cmd


data Config model msg =
    Config
        (model, Cmd msg)
        (msg -> model -> (model, Cmd msg))
        (model -> Sub msg)


merge :: Cmd msg -> (model, Cmd msg) -> (model, Cmd msg)
merge c1 (m, c2) = (m, Cmd.batch [c1, c2])


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


runCmds
    :: Map.Map Int (T.CmdQ, ThreadId)
    -> TQueue (T.TCmd msg)
    -> T.Config model msg
    -> IO ()
runCmds writeS sink cfg = do
    (msgs) <- execCmd writeS sink cmd

    case msgs of
       [] -> return ()
       _ -> atomically (foldMsgsStm (T.update cfg) msgs tmodel) >>=
           \cmds -> runCmds writeS sink $ cfg { T.init = (tmodel, cmds) }

    where
        (tmodel, cmd) = T.init cfg


run2 :: InternalState msg -> T.Config model msg -> IO ()
run2 self cfg = do
    writeS <- readTVarIO $ writeThreadS self
    runCmds writeS (cmdSink self) cfg

    -- TODO: consider moving this inside of updateSubscriptions and removing
    -- the Sub argument
    --model <- readTVarIO $ fst $ T.init cfg

    newself <-
        updateSubscriptions
            cfg
            self

    return () -- Here we need to await subs or cmds


run :: Config model msg -> IO ()
run (Config (m, cmd) fupdate subs) = do
    (tmodel, writeS, subsink, cmdsink) <- atomically $ do
        tmodel <- newTVar m
        writeS <- newTVar Map.empty
        subsink <- newEmptyTMVar
        cmdsink <- newTQueue
        return (tmodel, writeS, subsink, cmdsink)

    let cfg = T.Config (tmodel, cmd) fupdate subs

    run2
        ( InternalState
            Map.empty
            writeS
            Map.empty
            subsink
            cmdsink
        )
        cfg
