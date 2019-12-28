import Control.Concurrent (forkIO)

import Network.KRPC.Types (Port)
import qualified Mainline.App as App

ports :: [Port]
ports = [51417..51424]

getQuit :: IO ()
getQuit = getLine >>= \l -> if l == "quit" then return () else getQuit

main :: IO ()
main = mapM_ (forkIO . App.main) ports >> getQuit
