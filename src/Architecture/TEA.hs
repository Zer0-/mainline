module Architecture.TEA
    ( Cmd
    , Config (Config)
    , run
    ) where

import Architecture.Cmd (Cmd)


data TSub msg = SubNothing

newtype Sub msg = Sub [ TSub msg ]


data Config model msg =
    Config
    { init          :: (model, Cmd msg)
    , update :: msg -> model -> (model, Cmd msg)
    --, subscriptions :: model -> Sub msg
    }


run :: Config model msg -> IO ()
run = undefined
