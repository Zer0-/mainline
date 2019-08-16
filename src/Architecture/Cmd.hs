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
    ) where

import Prelude hiding (log, print, readFile, writeFile)
import Data.List (intercalate)
import Data.ByteString (ByteString)
import Data.Time.Clock.POSIX (POSIXTime)
import Architecture.Internal.Cmd (batch) -- for export only

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
minLoglvl = fromEnum DEBUG


getRandom :: (Float -> msg) -> Cmd msg
getRandom f = Cmd [ CmdGetRandom f ]

randomBytes :: Int -> (ByteString -> msg) -> Cmd msg
randomBytes n f = Cmd [ CmdRandomBytes n f ]

none :: Cmd msg
none = Cmd []

sendUDP :: Port -> CompactInfo -> ByteString -> Cmd msg
sendUDP p dest bs = Cmd [ CmdSendUDP p dest bs ]

sendTCP :: CompactInfo -> ByteString -> Cmd msg
sendTCP ci bs = Cmd [ CmdSendTCP ci bs ]

getTime :: (POSIXTime -> msg) -> Cmd msg
getTime f = Cmd [ CmdGetTime f ]

data Loglevel = WARNING | INFO | DEBUG deriving (Enum, Show)

log :: Loglevel -> [ String ] -> Cmd msg
log lvl xs
    | fromEnum lvl <= minLoglvl = Cmd [CmdLog s]
    | otherwise = none
    where
        s = show lvl ++ " - "
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
