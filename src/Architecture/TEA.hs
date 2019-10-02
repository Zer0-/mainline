{-# LANGUAGE NamedFieldPuns #-}

module Architecture.TEA
    ( Program
    , dbApp
    , simpleApp
    ) where

import qualified Data.Map as Map
import Control.Concurrent.STM
    ( newTVar
    , readTVar
    , readTVarIO
    , newEmptyTMVar
    , takeTMVar
    , newTQueue
    , readTQueue
    , atomically
    , orElse
    )

import Prelude hiding (init)
--import Generics.SOP (K (..))
import Squeal.PostgreSQL.Pool (Pool, createConnectionPool, destroyAllResources)
import Data.ByteString (ByteString)

import Architecture.Internal.Cmd (runCmds, updateWriters)
import Architecture.Internal.Types
    ( InternalState (..)
    , Cmd (..)
    , Sub (..)
    , Program (..)
    )
import Architecture.Internal.Sub (updateSubscriptions)


loop :: InternalState msg schemas -> Program model msg -> IO ()
loop self cfg = do
    mthing <- atomically $ do
        writeS <- readTVar (writeThreadS self)

        if Map.null writeS && Map.null (readThreadS self) then
            getThing `orElse` return Nothing
        else getThing

    case mthing of
        Nothing -> maybe (return ()) destroyAllResources (dbPool self)
        Just thing -> do
            newself <- case thing of
                (Left tcmd) -> updateWriters tcmd self
                (Right sub) -> updateSubscriptions sub cfg self

            loop newself cfg

    where
        lexpr = readTQueue (cmdSink self) >>= return . Left
        rexpr = takeTMVar (subSink self) >>= return . Right
        getThing = (lexpr `orElse` rexpr) >>= return . Just


run2 :: InternalState msg schemas -> Program model msg -> IO ()
run2 self cfg = do
    writeS <- readTVarIO $ writeThreadS self
    runCmds writeS (cmdSink self) cfg

    model <- readTVarIO $ fst $ init cfg

    newself <-
        updateSubscriptions
            ((subscriptions cfg) model)
            cfg
            self

    loop newself cfg


run
    :: Maybe (Pool schemas)
    -> (model, Cmd msg)
    -> (msg -> model -> (model, Cmd msg))
    -> (model -> Sub msg)
    -> IO ()
run dbpool (m, cmd) fupdate subs = do
    (tmodel, writeS, subsink, cmdsink) <- atomically $ do
        tmodel <- newTVar m
        writeS <- newTVar Map.empty
        subsink <- newEmptyTMVar
        cmdsink <- newTQueue
        return (tmodel, writeS, subsink, cmdsink)

    let cfg = Program (tmodel, cmd) fupdate subs

    run2
        ( InternalState
            Map.empty -- readThreadS
            writeS    -- writeThreadS
            Map.empty -- sockets
            dbpool
            subsink
            cmdsink
        )
        cfg


simpleApp
    :: (model, Cmd msg)
    -> (msg -> model -> (model, Cmd msg))
    -> (model -> Sub msg)
    -> IO ()
simpleApp = run Nothing

dbApp
    :: (model, Cmd msg)
    -> (msg -> model -> (model, Cmd msg))
    -> (model -> Sub msg)
    -> ByteString
    -> IO ()
dbApp initial fupdate subs connstr = do
    pool <- createConnectionPool connstr 1 1 1
    run (Just pool) initial fupdate subs
