import Control.Concurrent (forkIO)

import qualified Mainline.App as App
import qualified Mainline.Config as Config

getQuit :: IO ()
getQuit = getLine >>= \l -> if l == "quit" then return () else getQuit

main :: IO ()
main = do
    settings <- Config.getConfig
    mapM_ (forkIO . (App.main settings)) (Config.udpPorts settings)
    getQuit
