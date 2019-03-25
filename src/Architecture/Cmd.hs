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
    , getTime
    , readFile
    , writeFile
    ) where

import Prelude hiding (log, print, readFile, writeFile)
import Data.List (intercalate)
import Data.ByteString (ByteString)
import Data.Time.Clock.POSIX (POSIXTime)

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

getTime :: (POSIXTime -> msg) -> Cmd msg
getTime f = Cmd [ CmdGetTime f ]

data Loglevel = WARNING | INFO | DEBUG deriving Show

log :: Loglevel -> [ String ] -> Cmd msg
log lvl xs = Cmd [CmdLog s]
    where
        s
            = show lvl ++ " - "
            ++ (intercalate " " xs)
            ++ "\n"

print :: Show a => a -> Cmd msg
print x = Cmd [ CmdLog s ]
    where
        s = (show x) ++ "\n"

readFile :: String -> (ByteString -> msg) -> Cmd msg
readFile filename f = Cmd [ CmdReadFile filename f ]

writeFile :: String -> ByteString -> Cmd msg
writeFile filename bs = Cmd [ CmdWriteFile filename bs ]
