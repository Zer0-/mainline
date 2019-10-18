{-# LANGUAGE DataKinds #-}

import Data.Word (Word32)

import qualified Architecture.Cmd as Cmd
import qualified Architecture.Sub as Sub
import Architecture.TEA (simpleApp)
import Network.KRPC.Helpers (stringpack)
import Network.KRPC.Types (Port, CompactInfo (..))
import Network.Octets (Octets (..))

servePort :: Port
servePort = 51412

seedNodePort :: Port
seedNodePort = 51411
--seedNodePort = 51412

seedNodeHost :: Word32
seedNodeHost = fromOctets [ 127, 0, 0, 1 ]

seedNodeInfo :: CompactInfo
seedNodeInfo = CompactInfo seedNodeHost seedNodePort


update :: b -> a -> (a, Cmd.Cmd b '[])
update _ _ = (undefined, Cmd.none)

main :: IO ()
main = simpleApp (undefined, cmd) update (\_ -> Sub.none)
    where
        cmd =
            Cmd.sendUDP
                servePort
                seedNodeInfo
                (stringpack "hello world")
                (error "connection failed")
