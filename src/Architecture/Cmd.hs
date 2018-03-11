module Architecture.Cmd
    ( Cmd
    , getRandom
    , exit
    ) where

import System.Random (randomIO)

data TCmd msg
    = CmdNone
    | CmdLog String
    | CmdGetRandom (Float -> msg)
    | CmdExit

newtype Cmd msg = Cmd [ TCmd msg ]

getRandom :: (Float -> msg) -> Cmd msg
getRandom f = Cmd [ CmdGetRandom f ]

exit :: Cmd msg
exit = Cmd [ CmdExit ]
