{-# LANGUAGE NamedFieldPuns #-}

module Architecture.TEA
    ( Config (Config)
    , run
    ) where

import Prelude hiding (init)
import qualified Data.Map as Map
import Architecture.Internal.Cmd (Cmd (Cmd), execCmd)
import Architecture.Internal.Types (InternalState (..), SubState)
import Architecture.Internal.Sub
    ( Sub (..)
    , updateSubscriptions
    , readSubscriptions
    )
import qualified Architecture.Cmd as Cmd


data Config model msg =
    Config
    { init          :: (model, Cmd msg)
    , update        :: msg -> model -> (model, Cmd msg)
    , subscriptions :: model -> Sub msg
    }


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


runCmds
    :: InternalState msg
    -> Config model msg
    -> IO (InternalState msg, Config model msg)
runCmds states cfg =
    do
       (states2, msgs) <- execCmd states cmd

       case msgs of
           [] -> return (states2, cfg)
           _ -> runCmds states2 $ cfg { init = foldMsgs (update cfg) msgs model }
    where
        (model, cmd) = init cfg


updateModelWithSubMsgs
    :: SubState msg
    -> Config model msg
    -> IO (SubState msg, Config model msg)
updateModelWithSubMsgs substates cfg =
    do
      (newSubStates, msgs) <- readSubscriptions substates

      return $
          ( newSubStates
          , cfg { init = foldMsgs (update cfg) msgs (fst $ init cfg) }
          )


run2 :: InternalState msg -> Config model msg -> IO ()
run2 internalState cfg =
    do
        (internalState2, newcfg) <- runCmds internalState cfg

        substates <-
            updateSubscriptions
                (subState internalState2)
                ((subscriptions cfg) (fst $ init newcfg))

        case Map.null substates of
            True -> return ()
            False ->
                updateModelWithSubMsgs substates newcfg
                >>= \(s, c) -> run2 (internalState2 { subState = s }) c

run :: Config model msg -> IO ()
run = run2 (InternalState Map.empty)
