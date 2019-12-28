module Mainline.Prepopulate
    (main) where

import Prelude hiding (init)
import Data.Word (Word32)
import Data.Array (Array, listArray)

import Control.Concurrent.Chan.Unagi (InChan, newChan, getChanContents)

import Architecture.TEA (simpleApp)
import Architecture.Sub (Sub)
import qualified Architecture.Cmd as Cmd
import qualified Architecture.Sub as Sub
import qualified Mainline.Mainline as M
import Network.KRPC.Types (CompactInfo (..), Port)
import Network.Octets (Octets (..))

nFind :: Int
nFind = 1000

bucketSize :: Int
bucketSize = 128

seedNodePort :: Port
seedNodePort = 6881
--seedNodePort = 51413

seedNodeHost :: Word32
seedNodeHost = fromOctets [ 82, 221, 103, 244 ]
--seedNodeHost = fromOctets [ 192, 168, 4, 2 ]
--seedNodeHost = fromOctets [ 67, 215, 246, 10 ]

seedNodeInfo :: CompactInfo
seedNodeInfo = CompactInfo seedNodeHost seedNodePort

seedNodes :: Array Int CompactInfo
seedNodes = listArray (0, 0) [seedNodeInfo];

data Model = Model
    { model :: M.Model
    , count :: Int
    , writeQ :: InChan CompactInfo
    }

init :: InChan CompactInfo -> Port -> (Model, M.Cmd M.Msg)
init inchan p = (m, Cmd.batch [logmsg, Cmd.randomBytes 20 (M.NewNodeId 0)])
    where
        m = Model (M.Uninitialized cfg) 0 inchan
        logmsg = Cmd.log Cmd.INFO [ "Prepopulate init" ]
        cfg = M.ServerConfig 0 p undefined seedNodes bucketSize

subscriptions :: Model -> Sub M.Msg
subscriptions (Model { model = M.Uninitialized _}) = Sub.none
subscriptions (Model { model = m, count = n })
    | n > nFind = Sub.none
    | otherwise = Sub.udp (getPort m) M.parseReceivedBytes M.UDPError
    where
        getPort :: M.Model -> Port
        getPort (M.Uninitialized1 M.ServerConfig { M.listenPort = p } _) = p
        getPort
            ( M.Ready M.ServerState
                { M.conf = M.ServerConfig { M.listenPort = p } }
            ) = p
        getPort (M.Uninitialized _) = undefined

update :: M.Msg -> Model -> (Model, M.Cmd M.Msg)
update (M.PeersFoundResult _ _ _ _) m = (m, Cmd.none)
update (M.NodeAdded ci) m = (m { count = (count m) + 1 }, cmd)
    where
        cmd = Cmd.writeChan (writeQ m) ci

update msg m = (m { model = mm }, cmds)
    where
        (mm, cmds) = M.update msg (model m)

main :: Port -> IO [CompactInfo]
main p = do
    (ins, outs) <- newChan
    simpleApp (init ins p) update subscriptions
    getChanContents outs >>= return . take nFind
