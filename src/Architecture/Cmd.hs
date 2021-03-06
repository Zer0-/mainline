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
    , sendTCP
    , getTime
    , readFile
    , writeFile
    , db
    , up
    , bounce
    , writeChan
    ) where

import Prelude hiding (log, print, readFile, writeFile)
import Data.List (intercalate)
import Data.Hashable (Hashable)
import Data.ByteString (ByteString)
import Data.Time.Clock.POSIX (POSIXTime)
import Squeal.PostgreSQL (PQ)
import Control.Concurrent.Chan.Unagi (InChan)

import Architecture.Internal.Cmd (batch, mapTCmd)

import Network.KRPC.Types
    ( Port
    , CompactInfo
    )

import Architecture.Internal.Types
    ( TCmd (..)
    , Cmd (..)
    )

-- TODO: use a config
minLoglvl :: Int
minLoglvl = fromEnum INFO


getRandom :: (Float -> msg) -> Cmd msg schemas
getRandom f = Cmd [ CmdGetRandom f ]

randomBytes :: Int -> (ByteString -> msg) -> Cmd msg schemas
randomBytes n f = Cmd [ CmdRandomBytes n f ]

none :: Cmd msg schemas
none = Cmd []

sendUDP :: Port -> CompactInfo -> ByteString -> msg -> Cmd msg schemas
sendUDP p dest bs err = Cmd [ CmdSendUDP p dest bs err ]

sendTCP
    :: Hashable t
    => t
    -> CompactInfo
    -> ByteString
    -> msg
    -> Cmd msg schemas
sendTCP t ci bs err = Cmd [ CmdSendTCP t ci bs err ]

getTime :: (POSIXTime -> msg) -> Cmd msg schemas
getTime f = Cmd [ CmdGetTime f ]

data Loglevel = WARNING | INFO | DEBUG deriving (Enum, Show)

log :: Loglevel -> [ String ] -> Cmd msg schemas
log lvl xs
    | fromEnum lvl <= minLoglvl = Cmd [CmdLog s]
    | otherwise = none
    where
        s = show lvl ++ " - "
            ++ (intercalate " " xs)
            ++ "\n"

print :: Show a => a -> Cmd msg schemas
print x = Cmd [ CmdLog s ]
    where
        s = (show x) ++ "\n"

readFile :: String -> (ByteString -> msg) -> Cmd msg schemas
readFile filename f = Cmd [ CmdReadFile filename f ]

writeFile :: String -> ByteString -> Cmd msg schemas
writeFile filename bs = Cmd [ CmdWriteFile filename bs ]

db
    :: PQ schemas schemas IO result
    -> Either msg (Maybe result -> msg)
    -> Cmd msg schemas
db schemas f = Cmd [ CmdDatabase schemas f ]

up :: (msg0 -> msg1) -> Cmd msg0 schemas -> Cmd msg1 schemas
up f (Cmd xs) = Cmd $ map (mapTCmd f) xs

bounce :: msg -> Cmd msg schemas
bounce m = Cmd [ CmdBounce m ]

writeChan :: InChan a -> a -> Cmd msg schemas
writeChan c x = Cmd [ CmdWriteChan c x ]
