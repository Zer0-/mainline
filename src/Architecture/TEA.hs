{-# LANGUAGE NamedFieldPuns #-}

module Architecture.TEA
    ( Config (Config)
    , run
    ) where

import Prelude hiding (init)
import qualified Data.Map as Map
import Control.Concurrent.STM
    ( STM
    , TVar
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
    :: InternalState msg
    -> T.Config model msg
    -> IO (T.Config model msg)
runCmds self cfg =
    do
       (msgs) <- execCmd (writeThreadS self) (cmdSink self) cmd

       case msgs of
           [] -> return cfg
           _ -> atomically (foldMsgsStm (T.update cfg) msgs tmodel) >>=
               \cmds -> runCmds self $ cfg { T.init = (tmodel, cmds) }

    where
        (tmodel, cmd) = T.init cfg


run2 :: InternalState msg -> T.Config model msg -> IO ()
run2 self cfg =
    do
        newcfg <- runCmds self cfg

        model <- readTVarIO $ fst $ T.init newcfg

        newself <-
            updateSubscriptions
                newcfg
                self
                ((T.subscriptions cfg) model)

        return () -- Here we need to await subs or cmds


run :: Config model msg -> IO ()
run (Config (m, cmd) fupdate subs) = do
    (tmodel, subsink, cmdsink) <- atomically $ do
        tmodel <- newTVar m
        subsink <- newEmptyTMVar
        cmdsink <- newTQueue
        return (tmodel, subsink, cmdsink)

    let cfg = T.Config (tmodel, cmd) fupdate subs

    run2
        ( InternalState
            Map.empty
            Map.empty
            Map.empty
            subsink
            cmdsink
        )
        cfg
