import Control.Concurrent (forkIO)
import Data.ByteString (ByteString)

import Squeal.PostgreSQL.Pool (Pool, createConnectionPool)
import Squeal.PostgreSQL (Connection)
import Generics.SOP (K (..))

import qualified Mainline.App as App
import qualified Mainline.Config as Config
import Mainline.SQL (Schemas)

getQuit :: IO ()
getQuit = getLine >>= \l -> if l == "quit" then return () else getQuit

createPool :: Int -> ByteString -> IO (Pool (K Connection Schemas))
createPool n connstr = createConnectionPool connstr 1 1 n

main :: IO ()
main = do
    settings <- Config.getConfig
    print settings

    pool <- mkPool settings

    mapM_
      (forkIO . (App.main settings $ return pool))
      (Config.udpPorts settings)
    getQuit

    where
      mkPool s = createPool (Config.sqlConnPoolSize s) (Config.sqlConnStr s)
