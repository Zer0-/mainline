module Network.KRPC.Types
    ( NodeID
    , InfoHash
    , Port
    , CompactInfo (..)
    , NodeInfo    (..)
    , Message     (..)
    , bEncode
    ) where

import Data.Word        (Word16, Word32)
import Data.Digest.SHA1 (Word160)
import Data.List        (intercalate)
import Data.ByteString  (ByteString)
import Data.BEncode     (BEncode (..), BValue)

import Network.KRPC.Helpers (hexify)
import Network.Octets   (Octets (..), octToByteString)

type NodeID = Word160

type InfoHash = Word160

type Port = Word16

type Token = ByteString




data CompactInfo = CompactInfo
    { ip    :: Word32
    , port  :: Port
    } deriving Eq


instance Octets CompactInfo where
    octets (CompactInfo i p) = octets i ++ octets p

    fromOctets bytes = CompactInfo ip_ port_
        where
            ip_   = fromOctets $ take 4 bytes
            port_ = fromOctets $ take 2 (drop 4 bytes)


instance Show CompactInfo where
    show (CompactInfo i p) =
        "<" ++ ipstr ++ ":" ++ show p ++ ">"
        where
            ipstr = intercalate "." $ map show (octets i)


instance BEncode CompactInfo where
    toBEncode = bEncode
    fromBEncode = undefined




data NodeInfo = NodeInfo
    { nodeId       :: NodeID
    , compactInfo  :: CompactInfo
    } deriving Eq


instance Show NodeInfo where
    show (NodeInfo nodeid compactinfo) =
        "<Node " ++ showId nodeid ++ " " ++ show compactinfo ++ ">"


instance Octets NodeInfo where
    octets (NodeInfo nId nInfo) = octets nId ++ octets nInfo

    fromOctets bytes = NodeInfo nodeId_ compactInfo_
        where
            nodeId_      = fromOctets $ take 20 bytes
            compactInfo_ = fromOctets $ drop 20 bytes


instance BEncode NodeInfo where
    toBEncode = bEncode
    fromBEncode = undefined




data Message
    = Query NodeID Message
    | Response NodeID Message
    | Ping
    | FindNode NodeID
    | Nodes [NodeInfo]
    | AskPeers InfoHash
    | PeersFound Token Message
    | Values [CompactInfo]
    | AnnouncePeer InfoHash Port Token Bool -- last arg is implied_port
    | Error { errCode :: Integer, errMsg :: ByteString }
    deriving Eq


instance Show Message where
    show (Error code msg) =
        "<Error code: " ++ show code ++ ", "
            ++ "msg: "  ++ show msg ++ ">"

    show (Query nid msg) =
        "<Query from: " ++ showId nid
            ++ " msg: " ++ show msg ++ ">"

    show (Response nid msg) =
        "<Response from: " ++ showId nid
            ++ " msg: " ++ show msg ++ ">"

    show (FindNode nid) = "<FindNode " ++ showId nid ++ ">"

    show (Nodes ns) = "<Nodes " ++ intercalate "," (map show ns) ++ ">"

    show (PeersFound t msg) =
        "PeersFound{token: " ++ show t ++ " " ++ show msg ++ "}"

    show (Values v) = "Values" ++ show v

    show Ping = "Ping"

    show _ = ""


octToString :: (Octets a) => a -> String
octToString = hexify . octets

showId :: Octets a => a -> String
showId = (take 8) . octToString

bEncode :: (Octets a) => a -> BValue
bEncode = toBEncode . octToByteString
