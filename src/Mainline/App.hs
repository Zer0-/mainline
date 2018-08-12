import Prelude hiding (init)
import Network.Socket (SockAddr (..))
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import Data.Word                     (Word32)
import Data.Digest.SHA1              (Word160 (..))
import Data.BEncode                  (encode, decode)
import Crypto.Random                 (newGenIO, genBytes)
import Crypto.Random.DRBG            (CtrDRBG)

import Architecture.TEA              (Config (..), run)
import qualified Architecture.Cmd as Cmd
import qualified Architecture.Sub as Sub
import Architecture.Sub (Sub)
import Network.KRPC                  (KPacket (..))
import Network.KRPC.Helpers          (stringpack)
import Network.Octets                (Octets (..), fromByteString)
import Mainline.Bucket               (RoutingTable (Bucket))
import Network.KRPC.Types
    ( Port
    , CompactInfo (..)
    , Message (..)
    , NodeID
    )
import Mainline.Mainline
    ( ServerState (..)
    , Outbound
    , serverHandler
    , NotImplemented (..)
    )

seedNodePort :: Port
seedNodePort = 51413

seedNodeHost :: Word32
seedNodeHost = fromOctets [192, 168, 4, 2]

seedNodeInfo :: CompactInfo
seedNodeInfo = CompactInfo seedNodeHost seedNodePort

tidsize :: Int
tidsize = 128

--Maximum size of a bucket in the Routing Table before it must be split
bucketsize :: Int
bucketsize = 8

word160FromString :: String -> Word160
word160FromString = fromByteString . stringpack

lazyBStoStrict :: BL.ByteString -> BS.ByteString
lazyBStoStrict = BS.concat . BL.toChunks

compactInfoFromSockAddr :: SockAddr -> Maybe CompactInfo
compactInfoFromSockAddr (SockAddrInet port_ host) =
    Just $ CompactInfo host (fromIntegral port_)
compactInfoFromSockAddr _ = Nothing

genTid :: IO (BS.ByteString)
genTid = do
    g <- newGenIO :: IO CtrDRBG
    case genBytes tidsize g of
        Left err -> error $ show err
        Right (result, _) -> return result


createInitialState :: NodeID -> ServerState
createInitialState ourNodeId = ServerState Map.empty ourNodeId rt
    where
        rt = Bucket ourNodeId bucketsize minword maxword Set.empty
        minword = Word160 i i i i i
        maxword = Word160 j j j j j
        i = minBound :: Word32
        j = maxBound :: Word32

data Msg
    = NewNodeId BS.ByteString
    | ReceiveBytes SockAddr BS.ByteString

init :: (ServerState, Cmd.Cmd Msg)
init = (Uninitialized, Cmd.randomBytes 160 NewNodeId)

update :: Msg -> ServerState -> (ServerState, Cmd.Cmd Msg)
update (NewNodeId bs) _ = (createInitialState (fromByteString bs), Cmd.none)

subscriptions :: ServerState -> Sub Msg
subscriptions _ = Sub.none

config :: Config ServerState Msg
config = Config init update subscriptions

main :: IO ()
main = run config
