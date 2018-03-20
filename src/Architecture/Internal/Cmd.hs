module Architecture.Internal.Cmd
    ( TCmd (..)
    , Cmd (..)
    , execCmd
    ) where

import System.Random (randomIO)
import Data.Maybe (catMaybes)


data TCmd msg
    = CmdLog String
    | CmdGetRandom (Float -> msg)


newtype Cmd msg = Cmd [ TCmd msg ]

execTCmd :: TCmd msg -> IO (Maybe msg)
execTCmd (CmdGetRandom f) = randomIO >>= (return . Just . f)
execTCmd (CmdLog msg) = putStr msg >> (return Nothing)

execCmd :: Cmd msg -> IO [ msg ]
execCmd (Cmd l) = (mapM execTCmd l) >>= (return . catMaybes)
