import Prelude hiding (init)
import Data.Word (Word32)
import Data.Hex (unhex, hex)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Serialize (encode, decode)
import Data.Default (def)
import Data.Maybe (fromJust)
import qualified Network.BitTorrent.Exchange.Message as BT
import qualified Network.BitTorrent.Address as BT
import qualified Data.Torrent as BT

import Network.KRPC.Helpers (stringpack)
import Network.KRPC.Types (Port, CompactInfo(..), NodeID)
import Network.Octets (fromOctets, fromByteString)
import Architecture.TEA (Config (..), run)
import Architecture.Sub (Sub, Received(..))
import qualified Architecture.Sub as Sub
import qualified Architecture.Cmd as Cmd
import Architecture.Cmd (Cmd)

knownNodeHost :: Word32
knownNodeHost = fromOctets [ 192, 168, 4, 2 ]

knownNodePort :: Port
knownNodePort = 51413

knownNodeInfo :: CompactInfo
knownNodeInfo = CompactInfo knownNodeHost knownNodePort

testHash :: String
testHash = "139945d35fc1751c6bd144f0de7b5a124d09df79"

data Model
    = Off
    | Headers NodeID
    -- | ExtensionHeaders NodeID

data Msg
    = NewNodeID ByteString
    | Got Received

config :: Config Model Msg
config = Config init update subscriptions

init :: (Model, Cmd Msg)
init = (Off, Cmd.randomBytes 20 NewNodeID)

update :: Msg -> Model -> (Model, Cmd Msg)
update (NewNodeID bs) Off = (Headers (fromByteString bs), Cmd.batch [ logmsg, sendHandshake ])
    where
        logmsg = Cmd.log Cmd.DEBUG [ "Hello World", show $ hex bs ]
        sendHandshake = Cmd.sendTCP knownNodeInfo (encode handshake)
        handshake = BT.Handshake
            def
            (BT.toCaps [ BT.ExtExtended ])
            (BT.InfoHash $ fromJust $ unhex $ stringpack testHash)
            (BT.PeerId bs)


update (Got m) (Headers _) = (Off, logmsg) --(ExtensionHeaders, cmds)
    where
        logmsg = Cmd.log Cmd.DEBUG [ "Have header", show handshake ]
        handshake :: Either String BT.Handshake
        handshake = decode $ bytes m

update _ Off = (Off, Cmd.none)

subscriptions :: Model -> Sub Msg
subscriptions Off = Sub.none
subscriptions (Headers _) = Sub.readTCP knownNodeInfo numToRead Got
    where
        numToRead :: ByteString -> Int
        numToRead bs
            = 1  -- length prefix
            + 19 -- "BitTorrent protocol"
            + 8  -- reserved
            + 20 -- InfoHash
            + 20 -- NodeID
            - BS.length bs

main :: IO ()
main = run config
