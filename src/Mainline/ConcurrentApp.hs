import Control.Concurrent (forkIO)
import Data.ByteString (ByteString)

import Squeal.PostgreSQL.Pool (Pool, createConnectionPool)
import Squeal.PostgreSQL (Connection)
import Generics.SOP (K (..))

import qualified Mainline.App as App
import qualified Mainline.Config as Conf
import Mainline.SQL (Schemas, runSetup)

getQuit :: IO ()
getQuit = getLine >>= \l -> if l == "quit" then return () else getQuit

createPool :: Int -> ByteString -> IO (Pool (K Connection Schemas))
createPool n connstr = createConnectionPool connstr 2 60 n

main :: IO ()
main = do
    settings <- Conf.getConfig
    print settings

    runSetup $ Conf.sqlConnStr settings

    pool <- mkPool settings

    mapM_
      (forkIO . (App.main settings $ return pool))
      (Conf.udpPorts settings)
    getQuit

    where
      mkPool s = createPool (Conf.sqlConnPoolSize s) (Conf.sqlConnStr s)
