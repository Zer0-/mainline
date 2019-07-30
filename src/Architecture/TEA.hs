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
    , readTVar
    , readTVarIO
    , writeTVar
    , atomically
    )

import Architecture.Internal.Cmd (execCmd)
import Architecture.Internal.Types
    ( InternalState (..)
    , SubState
    , Cmd (..)
    , Sub (..)
    )
import qualified Architecture.Internal.Types as T
import Architecture.Internal.Sub
    ( updateSubscriptions
    , readSubscriptions
    )
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
    -> IO (InternalState msg, T.Config model msg)
runCmds states cfg =
    do
       (states2, msgs) <- execCmd states cmd

       case msgs of
           [] -> return (states2, cfg)
           _ -> atomically (foldMsgsStm (T.update cfg) msgs tmodel) >>=
               \cmds -> runCmds states2 $ cfg { T.init = (tmodel, cmds) }

    where
        (tmodel, cmd) = T.init cfg


updateModelWithSubMsgs
    :: SubState msg
    -> T.Config model msg
    -> IO (SubState msg, T.Config model msg)
updateModelWithSubMsgs substates cfg =
    do
        -- This will have to be done in each individual thread for just one sub
        (newSubStates, msgs) <- readSubscriptions substates

        cmds <- atomically (foldMsgsStm (T.update cfg) msgs tmodel)

        return $
          ( newSubStates
          , cfg { T.init = (tmodel, Cmd.batch [cmd, cmds]) }
          )

    where
        (tmodel, cmd) = T.init cfg


run2 :: InternalState msg -> T.Config model msg -> IO ()
run2 internalState cfg =
    do
        (internalState2, newcfg) <- runCmds internalState cfg

        model <- readTVarIO $ fst $ T.init newcfg

        substates <-
            updateSubscriptions
                (subState internalState2)
                ((T.subscriptions cfg) model)

        case Map.null substates of
            True -> return ()
            False ->
                updateModelWithSubMsgs substates newcfg
                >>= \(s, c) -> run2 (internalState2 { subState = s }) c

mkInternalCfg :: Config model msg -> IO (T.Config model msg)
mkInternalCfg (Config (m, cmd) fupdate subs) = do
    tmodel <- atomically (newTVar m)
    return $ T.Config (tmodel, cmd) fupdate subs

run :: Config model msg -> IO ()
run cfg = mkInternalCfg cfg >>= (run2 (InternalState Map.empty))
