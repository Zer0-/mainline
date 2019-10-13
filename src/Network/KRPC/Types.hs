module Network.KRPC.Types
    ( NodeID
    , InfoHash
    , Port
    , CompactInfo (..)
    , NodeInfo    (..)
    , Message     (..)
    , QueryDat    (..)
    , ResponseDat (..)
    , bEncode
    ) where

import Data.Word        (Word16, Word32)
import Data.Hashable    (Hashable (..))
import Data.List        (intercalate)
import Data.ByteString  (ByteString)
import Data.BEncode     (BEncode (..), BValue)

import Network.Octets   (Octets (..), octToByteString)

type NodeID = Integer

type InfoHash = Integer

type Port = Word16

type Token = ByteString

data CompactInfo = CompactInfo
    { ip    :: Word32
    , port  :: Port
    } deriving (Eq, Ord)


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

instance Hashable CompactInfo where
  hashWithSalt s (CompactInfo i p) = s `hashWithSalt` i `hashWithSalt` p


instance BEncode CompactInfo where
    toBEncode = bEncode
    fromBEncode = undefined


data NodeInfo = NodeInfo
    { nodeId       :: NodeID
    , compactInfo  :: CompactInfo
    } deriving Eq


instance Show NodeInfo where
    show (NodeInfo nodeid compactinfo) =
        "<Node " ++ show nodeid ++ " " ++ show compactinfo ++ ">"


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
    = Query NodeID QueryDat
    | Response NodeID ResponseDat
    | Error { errCode :: Integer, errMsg :: ByteString }
    deriving Eq


data QueryDat
    = Ping
    | FindNode NodeID
    | GetPeers InfoHash
    | AnnouncePeer Bool InfoHash Port Token (Maybe ByteString)-- for fun, change Bool to Maybe Bool since it's an optional field
    deriving Eq


data ResponseDat
    = Pong
    | Nodes [NodeInfo]
    | PeersFound Token [CompactInfo]
    | NodesFound Token [NodeInfo]
    deriving Eq


instance Show Message where
    show (Error code msg) =
        "<Error code: " ++ show code ++ ", "
            ++ "msg: "  ++ show msg ++ ">"

    show (Query nid msg) =
        "<Query from: " ++ show nid
            ++ " msg: " ++ show msg ++ ">"

    show (Response nid msg) =
        "<Response from: " ++ show nid
            ++ " msg: " ++ show msg ++ ">"

instance Show QueryDat where
    show Ping = "<Ping>"

    show (FindNode nid) = "<FindNode " ++ show nid ++ ">"

    show (GetPeers ifo) = "<GetPeers info_hash:" ++ show ifo ++ ">"

    show (AnnouncePeer b ifo p token mname)
        = "<AnnouncePeer implied_port: " ++ show b
        ++ " infohash: " ++ show ifo
        ++ " port: " ++ show p
        ++ " token: " ++ show token
        ++ maybe "" (((++) "name: ") . show) mname
        ++ ">"

instance Show ResponseDat where
    show (Pong) = "<Pong>"

    show (Nodes ns) = "<Nodes " ++ intercalate "," (map show ns) ++ ">"

    show (PeersFound t ifo) =
        "<PeersFound token: " ++ show t ++ " " ++ show ifo ++ ">"

    show (NodesFound t nodes) =
        "<NodesFound token: " ++ show t ++ " " ++ show nodes ++ ">"


bEncode :: (Octets a) => a -> BValue
bEncode = toBEncode . octToByteString
