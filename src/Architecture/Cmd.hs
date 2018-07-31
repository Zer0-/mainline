{-# LANGUAGE ExistentialQuantification #-}

module Architecture.Cmd
    ( Cmd
    , getRandom
    , none
    , batch
    , Loglevel (..)
    , log
    , print
    ) where

import Prelude hiding (log, print)
import Data.List (intercalate)

import Architecture.Internal.Cmd
    ( TCmd (..)
    , Cmd (..)
    )

getRandom :: (Float -> msg) -> Cmd msg
getRandom f = Cmd [ CmdGetRandom f ]

none :: Cmd msg
none = Cmd []

batch :: [ Cmd msg ] -> Cmd msg
batch [] = Cmd []
batch ((Cmd t):xs) = Cmd (t ++ u)
    where
        (Cmd u) = batch xs


data Loglevel = WARNING | INFO | DEBUG deriving Show

log :: forall a msg. Show a => Loglevel -> [ a ] -> Cmd msg
log lvl xs = Cmd [CmdLog s]
    where
        s
            = "[" ++ (show lvl) ++ "] - "
            ++ (intercalate " " $ map show xs)
            ++ "\n"

print :: Show a => a -> Cmd msg
print x = Cmd [ CmdLog s ]
    where
        s = (show x) ++ "\n"
