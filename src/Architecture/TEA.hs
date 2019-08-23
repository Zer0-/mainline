{-# LANGUAGE NamedFieldPuns #-}

module Architecture.TEA
    ( Config (Config)
    , run
    ) where

import qualified Data.Map as Map
import Control.Concurrent.STM
    ( newTVar
    , readTVarIO
    , newEmptyTMVar
    , takeTMVar
    , newTQueue
    , readTQueue
    , atomically
    , orElse
    )

import Architecture.Internal.Cmd (runCmds, updateWriters)
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


loop :: InternalState msg -> T.Config model msg -> IO ()
loop self cfg = do
    thing <- atomically $ lexpr `orElse` rexpr

    newself <- case thing of
        (Left tcmd) -> updateWriters tcmd self
        (Right sub) -> updateSubscriptions sub cfg self

    loop newself cfg

    where
        lexpr = readTQueue (cmdSink self) >>= return . Left
        rexpr = takeTMVar (subSink self) >>= return . Right


run2 :: InternalState msg -> T.Config model msg -> IO ()
run2 self cfg = do
    writeS <- readTVarIO $ writeThreadS self
    runCmds writeS (cmdSink self) cfg

    model <- readTVarIO $ fst $ T.init cfg

    newself <-
        updateSubscriptions
            ((T.subscriptions cfg) model)
            cfg
            self

    loop newself cfg


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
