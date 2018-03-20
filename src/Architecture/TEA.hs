{-# LANGUAGE NamedFieldPuns #-}

module Architecture.TEA
    ( Config (Config)
    , run
    ) where

import Prelude hiding (init)
import Architecture.Internal.Cmd (Cmd (Cmd), execCmd)
import qualified Architecture.Cmd as Cmd


--data TSub msg = SubNothing


--newtype Sub msg = Sub [ TSub msg ]


data Config model msg =
    Config
    { init          :: (model, Cmd msg)
    , update :: msg -> model -> (model, Cmd msg)
    --, subscriptions :: model -> Sub msg
    }


merge :: Cmd msg -> (model, Cmd msg) -> (model, Cmd msg)
merge c1 (m, c2) = (m, Cmd.batch [c1, c2])


foldMsgs :: (msg -> model -> (model, Cmd msg)) -> [ msg ] -> model -> (model, Cmd msg)
foldMsgs _ [] mdl = (mdl, Cmd [])
foldMsgs f (x:xs) mdl = cmd2 `merge` foldMsgs f xs mdl2
    where
        (mdl2, cmd2) = f x mdl


run :: Config model msg -> IO ()
run (Config { init, update }) =
    do
       msgs <- execCmd initialCmd
       case msgs of
           [] -> return ()
           _ -> run $ Config (foldMsgs update msgs initialModel) update
    where
        (initialModel, initialCmd) = init
