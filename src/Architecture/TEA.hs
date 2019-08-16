{-# LANGUAGE NamedFieldPuns #-}

module Architecture.TEA
    ( Config (Config)
    , run
    ) where

import qualified Data.Map as Map
import Control.Concurrent.STM
    ( newTVar
    , newEmptyTMVar
    , newTQueue
    , readTVarIO
    , atomically
    )

import Architecture.Internal.Cmd (runCmds)
import Architecture.Internal.Types
    ( InternalState (..)
    , Cmd (..)
    , Sub (..)
    )
import qualified Architecture.Internal.Types as T
import Architecture.Internal.Sub (updateSubscriptions)


data Config model msg =
    Config
        (model, Cmd msg)
        (msg -> model -> (model, Cmd msg))
        (model -> Sub msg)


run2 :: InternalState msg -> T.Config model msg -> IO ()
run2 self cfg = do
    writeS <- readTVarIO $ writeThreadS self
    runCmds writeS (cmdSink self) cfg

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
