{-# LANGUAGE ExistentialQuantification #-}

module Architecture.Cmd
    ( Cmd
    , getRandom
    , randomBytes
    , none
    , batch
    , Loglevel (..)
    , log
    , print
    , sendUDP
    ) where

import Prelude hiding (log, print)
import Data.List (intercalate)
import Data.ByteString (ByteString)

import Network.KRPC.Types (Port, CompactInfo)

import Architecture.Internal.Cmd
    ( TCmd (..)
    , Cmd (..)
    )

getRandom :: (Float -> msg) -> Cmd msg
getRandom f = Cmd [ CmdGetRandom f ]

randomBytes :: Int -> (ByteString -> msg) -> Cmd msg
randomBytes n f = Cmd [ CmdRandomBytes n f ]

none :: Cmd msg
none = Cmd []

batch :: [ Cmd msg ] -> Cmd msg
batch cmds = Cmd $ concat [t | (Cmd t) <- cmds]

sendUDP :: Port -> CompactInfo -> ByteString -> Cmd msg
sendUDP p dest bs = Cmd [ CmdSendUDP p dest bs ]


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
