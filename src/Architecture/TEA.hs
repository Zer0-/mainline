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
    , Sub (..)
    , updateSubscriptions
    , readSubscriptions
    )
import qualified Architecture.Cmd as Cmd

import Debug.Trace (trace)

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


runCmds :: SubStates msg -> Config model msg -> IO (SubStates msg, Config model msg)
runCmds states cfg =
    do
       (states2, msgs) <- execCmd states cmd

       case msgs of
           [] -> return (states2, cfg)
           _ -> runCmds states2 $ cfg { init = foldMsgs (update cfg) msgs model }
    where
        (model, cmd) = init cfg


updateModelWithSubMsgs
    :: SubStates msg
    -> Config model msg
    -> IO (SubStates msg, Config model msg)
updateModelWithSubMsgs substates cfg = do
  (states, msgs) <- readSubscriptions substates
  return $ (states, cfg { init = foldMsgs (update cfg) msgs (fst $ init cfg) })


{-
lenSubs :: Sub msg -> Int
lenSubs (Sub l) = length l
-}

run_ :: SubStates msg -> Config model msg -> IO ()
run_ substates cfg =
    do
        (substates2, newcfg) <- runCmds substates cfg
        substates3 <- updateSubscriptions substates2 $ (subscriptions cfg) (fst $ init newcfg)

        case Map.null (trace ("subs size: " ++ show (Map.size substates3)) substates3) of
            True -> return ()
            False -> updateModelWithSubMsgs substates3 newcfg >>= \(s, c) -> run_ s c

run :: Config model msg -> IO ()
run = run_ Map.empty
