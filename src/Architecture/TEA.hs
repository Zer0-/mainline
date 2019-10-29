{-# LANGUAGE
    NamedFieldPuns
  , KindSignatures
  , DataKinds
#-}

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
import Generics.SOP (K (..))
import Squeal.PostgreSQL.Pool (Pool, createConnectionPool, destroyConnectionPool)
import Squeal.PostgreSQL (Connection)
import Data.ByteString (ByteString)
import Network.Socket (close)

import Architecture.Internal.Cmd (runCmds, updateWriters)
import Architecture.Internal.Types
    ( InternalState (..)
    , Cmd (..)
    , Sub (..)
    , Program (..)
    , TCmd (..)
    , SocketMood (..)
    )
import Architecture.Internal.Sub (updateSubscriptions)

import Debug.Trace (trace)

loop :: InternalState msg schemas -> Program model msg schemas -> IO ()
loop self cfg = do
    putStrLn "Loop"

    mthing <- atomically $ do
        writeS <- readTVar (writeThreadS self)

        if Map.null writeS && Map.null (readThreadS self) then
            getThing `orElse` return Nothing
        else getThing

    case mthing of
        Nothing -> maybe (return ()) destroyConnectionPool (dbPool self)
        Just thing -> do
            putStrLn "Loop has thing"
            newself <- case thing of
                (Left (SocketResult key Nothing)) ->
                    return $ self { sockets = Map.delete key (sockets self) }
                (Left (SocketResult key (Just s))) ->
                    case (Map.lookup key (sockets self)) of
                        Nothing -> close s >> return self

                        Just (HaveSocket _) ->
                            error "Newly opened socket collision"

                        Just (WantWrites (x:xs)) -> do
                            let self2 = putsocketm key (HaveSocket s)

                            self3 <- updateWriters x self2 cfg

                            runCmds self3 (cfg
                                { init = (\(tm, _) -> (tm, Cmd xs)) (init cfg) })

                            return self3

                        Just (WantWrites []) ->
                            error "No writes queued for new socket"

                        Just (WantReads _ _) -> do
                            model <- readTVarIO $ fst $ init cfg

                            updateSubscriptions
                                ((subscriptions cfg) model)
                                cfg
                                (putsocketm key (HaveSocket s))

                        Just (WantBoth ((x:xs), _)) -> do
                            -- TODO: this is just a copy paste of the previous
                            -- two conditions, do not repeat yourself
                            model <- readTVarIO $ fst $ init cfg

                            let self2 = putsocketm key (HaveSocket s)
                            self3 <- updateWriters x self2 cfg
                            runCmds self3 (cfg
                                { init = (\(tm, _) -> (tm, Cmd xs)) (init cfg) })

                            updateSubscriptions
                                ((subscriptions cfg) model)
                                cfg
                                self3

                        Just (WantBoth ([], _)) ->
                            error "No writes queued for new socket"

                (Left tcmd) -> trace "t mainloop updateWriters" $
                    updateWriters tcmd self cfg
                (Right sub) -> trace "t mainloop updateSubscriptions" $
                    updateSubscriptions sub cfg self

            putStrLn "looping"

            loop newself cfg

    where
        lexpr = trace "t mainloop - read TQueue" $ readTQueue (cmdSink self) >>= return . Left
        rexpr = trace "t mainloop - read TMVar" $ takeTMVar (subSink self) >>= return . Right
        getThing = (lexpr `orElse` rexpr) >>= return . Just
        putsocketm key s = self
            { sockets = Map.insert key s (sockets self) }


run2 :: InternalState msg schemas -> Program model msg schemas -> IO ()
run2 self cfg = do
    runCmds self cfg

    model <- readTVarIO $ fst $ init cfg

    newself <-
        updateSubscriptions
            ((subscriptions cfg) model)
            cfg
            self

    loop newself cfg


run
    :: Maybe (Pool (K Connection schemas))
    -> (model, Cmd msg schemas)
    -> (msg -> model -> (model, Cmd msg schemas))
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
    :: (model, Cmd msg schemas)
    -> (msg -> model -> (model, Cmd msg schemas))
    -> (model -> Sub msg)
    -> IO ()
simpleApp = run Nothing

dbApp
    :: (model, Cmd msg schemas)
    -> (msg -> model -> (model, Cmd msg schemas))
    -> (model -> Sub msg)
    -> ByteString
    -> IO ()
dbApp initial fupdate subs connstr = do
    pool <- createConnectionPool connstr 1 1 10
    run (Just pool) initial fupdate subs
