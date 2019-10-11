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
    ) where

import Prelude hiding (log, print, readFile, writeFile)
import Data.List (intercalate)
import Data.ByteString (ByteString)
import Data.Time.Clock.POSIX (POSIXTime)
import Squeal.PostgreSQL.Pool (PoolPQ)

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


getRandom :: (Float -> msg) -> Cmd msg schemas
getRandom f = Cmd [ CmdGetRandom f ]

randomBytes :: Int -> (ByteString -> msg) -> Cmd msg schemas
randomBytes n f = Cmd [ CmdRandomBytes n f ]

none :: Cmd msg schemas
none = Cmd []

sendUDP :: Port -> CompactInfo -> ByteString -> Cmd msg schemas
sendUDP p dest bs = Cmd [ CmdSendUDP p dest bs ]

sendTCP :: CompactInfo -> ByteString -> Cmd msg schemas
sendTCP ci bs = Cmd [ CmdSendTCP ci bs ]

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


db :: PoolPQ schemas IO result -> Maybe (result -> msg) -> Cmd msg schemas
db schemas f = Cmd [ CmdDatabase schemas f ]

mapTCmd :: (msg0 -> msg1) -> TCmd msg0 schemas -> TCmd msg1 schemas
mapTCmd _ (CmdLog a) = CmdLog a
mapTCmd f (CmdGetRandom h) = CmdGetRandom (f . h)
mapTCmd f (CmdGetTime h) = CmdGetTime (f . h)
mapTCmd f (CmdRandomBytes n h) = CmdRandomBytes n (f . h)
mapTCmd _ (CmdSendUDP p ci bs) = CmdSendUDP p ci bs
mapTCmd _ (CmdSendTCP ci bs) = CmdSendTCP ci bs
mapTCmd f (CmdReadFile p h) = CmdReadFile p (f . h)
mapTCmd _ (CmdWriteFile p bs) = CmdWriteFile p bs
mapTCmd f (CmdDatabase sesh (Just h)) = CmdDatabase sesh (Just $ f . h)
mapTCmd _ (CmdDatabase sesh Nothing) = CmdDatabase sesh Nothing
mapTCmd f (CmdBounce m) = CmdBounce (f m)
mapTCmd _ (QuitW i) = QuitW i

up :: (msg0 -> msg1) -> Cmd msg0 schemas -> Cmd msg1 schemas
up f (Cmd xs) = Cmd $ map (mapTCmd f) xs

bounce :: msg -> Cmd msg schemas
bounce m = Cmd [ CmdBounce m ]
