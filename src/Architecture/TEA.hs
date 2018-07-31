{-# LANGUAGE NamedFieldPuns #-}

module Architecture.TEA
    ( Config (Config)
    , run
    ) where

import Prelude hiding (init)
import qualified Data.Map as Map
import Architecture.Internal.Cmd (Cmd (Cmd), execCmd)
import Architecture.Internal.Sub
    ( SubStates
    , Sub
    , updateSubscriptions
    , readSubscriptions
    )
import qualified Architecture.Cmd as Cmd


data Config model msg =
    Config
    { init          :: (model, Cmd msg)
    , update :: msg -> model -> (model, Cmd msg)
    , subscriptions :: model -> Sub msg
    }


merge :: Cmd msg -> (model, Cmd msg) -> (model, Cmd msg)
merge c1 (m, c2) = (m, Cmd.batch [c1, c2])


foldMsgs :: (msg -> model -> (model, Cmd msg)) -> [ msg ] -> model -> (model, Cmd msg)
foldMsgs _ [] mdl = (mdl, Cmd [])
foldMsgs f (x:xs) mdl = cmd2 `merge` foldMsgs f xs mdl2
    where
        (mdl2, cmd2) = f x mdl


runCmds :: Config model msg -> IO (Config model msg)
runCmds cfg =
    do
       msgs <- execCmd cmd

       case msgs of
           [] -> return cfg
           _ -> runCmds $ cfg { init = foldMsgs (update cfg) msgs model }
    where
        (model, cmd) = init cfg


updateModelWithSubMsgs
    :: SubStates msg
    -> Config model msg
    -> IO (SubStates msg, Config model msg)
updateModelWithSubMsgs substates cfg = do
  (states, msgs) <- readSubscriptions substates
  return $ (states, cfg { init = foldMsgs (update cfg) msgs (fst $ init cfg) })


run_ :: SubStates msg -> Config model msg -> IO ()
run_ substates cfg =
    do
        newcfg <- runCmds cfg
        subs <- updateSubscriptions substates $ (subscriptions cfg) model

        case Map.null subs of
            True -> return ()
            False -> updateModelWithSubMsgs subs newcfg >>= \(s, c) -> run_ s c

    where
        (model, _) = init cfg

run :: Config model msg -> IO ()
run = run_ Map.empty
