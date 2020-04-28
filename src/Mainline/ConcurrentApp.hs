{-# LANGUAGE TemplateHaskell #-}

import Control.Concurrent (forkIO)
import Data.ByteString (ByteString)


import qualified Database.PostgreSQL.LibPQ as LibPQ
import Squeal.PostgreSQL.Pool (Pool, createConnectionPool)
import Squeal.PostgreSQL (Connection)
import Generics.SOP (K (..))
import Data.FileEmbed (embedFile)

import qualified Mainline.App as App
import qualified Mainline.Config as Conf
import Mainline.SQL (Schemas, runSetup)

parseTextDefinition :: ByteString
parseTextDefinition = $(embedFile "sql/plpgsql.sql")

getQuit :: IO ()
getQuit = getLine >>= \l -> if l == "quit" then return () else getQuit

createPool :: Int -> ByteString -> IO (Pool (K Connection Schemas))
createPool n connstr = createConnectionPool connstr 2 60 n

loadPLpgSQLFunction :: ByteString -> IO ()
loadPLpgSQLFunction connstr = do
    conn <- LibPQ.connectdb connstr
    _ <- LibPQ.exec conn parseTextDefinition
    LibPQ.finish conn

main :: IO ()
main = do
    settings <- Conf.getConfig
    print settings

    let connstr = Conf.sqlConnStr settings

    runSetup connstr
    loadPLpgSQLFunction connstr

    pool <- mkPool settings

    mapM_
      (forkIO . (App.main settings $ return pool))
      (Conf.udpPorts settings)

    getQuit

    where
      mkPool s = createPool (Conf.sqlConnPoolSize s) (Conf.sqlConnStr s)
