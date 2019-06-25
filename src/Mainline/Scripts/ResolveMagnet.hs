import Prelude hiding (init)
import Data.Word (Word32)
import Data.Hex (unhex, hex)
import Data.Map.Strict (singleton)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Serialize (encode, decode)
import Data.Default (def)
import Data.Maybe (fromJust)
import qualified Network.BitTorrent.Exchange.Message as BT
import qualified Network.BitTorrent.Address as BT
import qualified Data.Torrent as BT

import Network.KRPC.Helpers (stringpack)
import Network.KRPC.Types (Port, CompactInfo(..))
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
    | Handshake
    | ExtensionHandshake

data Msg
    = NewNodeID ByteString
    | Got Received

config :: Config Model Msg
config = Config init update subscriptions

init :: (Model, Cmd Msg)
init = (Off, Cmd.randomBytes 20 NewNodeID)

update :: Msg -> Model -> (Model, Cmd Msg)
update (NewNodeID bs) Off = (Handshake, Cmd.batch [ logmsg, sendHandshake ])
    where
        logmsg = Cmd.log Cmd.DEBUG [ "Hello World", show $ hex bs ]
        sendHandshake = Cmd.sendTCP knownNodeInfo (encode handshake)
        handshake = BT.Handshake
            def
            (BT.toCaps [ BT.ExtExtended ])
            (BT.InfoHash $ fromJust $ unhex $ stringpack testHash)
            (BT.PeerId bs)

update (Got m) Handshake = (ExtensionHandshake, Cmd.batch [ logmsg, sendCmd ])
    where
        logmsg = Cmd.log Cmd.DEBUG
            [ "Have handshake:", show handshake
            ,"Will send:", show eshake
            , show $ encode eshake
            ]

        sendCmd = Cmd.sendTCP knownNodeInfo (encode eshake)

        handshake :: Either String BT.Handshake
        handshake = decode $ bytes m

        -- TODO: check handshake for supporting BT.ExtExtended

        eshake :: BT.Message
        eshake = BT.Extended $ BT.EHandshake $
            BT.nullExtendedHandshake supportedExtensions

        supportedExtensions :: BT.ExtendedCaps
        supportedExtensions = BT.ExtendedCaps $ singleton BT.ExtMetadata 1

update (Got m) ExtensionHandshake = (Off, logmsg)
    where
        logmsg = Cmd.log Cmd.DEBUG [ "Have extended handshake", show $ bytes m, show eshake ]

        eshake :: Either String BT.Message
        eshake = decode $ bytes m

update _ Off = (Off, Cmd.none)


subscriptions :: Model -> Sub Msg
subscriptions Off = Sub.none
subscriptions Handshake = Sub.readTCP knownNodeInfo numToRead Got
    where
        numToRead :: ByteString -> Int
        numToRead bs
            = 1  -- length prefix
            + 19 -- "BitTorrent protocol"
            + 8  -- reserved
            + 20 -- InfoHash
            + 20 -- NodeID
            - BS.length bs

subscriptions ExtensionHandshake = Sub.readTCP knownNodeInfo numToRead Got
    where
        numToRead :: ByteString -> Int
        numToRead bs
            | BS.length bs < 4 = 4
            | otherwise = (expectedLen (BS.take 4 bs)) - (BS.length bs) + 4

        expectedLen :: ByteString -> Int
        expectedLen b = fromIntegral ((fromByteString b) :: Word32)

main :: IO ()
main = run config
