module KRPC where

import Data.Word
import Data.Bits
import Data.List (foldl')
import Data.ByteString (pack, unpack, ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.Digest.SHA1 (Word160 (Word160))
import Data.BEncode
import Data.BEncode.BDict

type NodeID = Word160

type InfoHash = Word160

type Port = Word16

data CompactInfo = CompactInfo
    { ip    :: Word32
    , port  :: Port
    }

instance Eq CompactInfo where
    CompactInfo i p == CompactInfo i' p' = i == i' && p == p'

data NodeInfo = NodeInfo
    { nodeId       :: NodeID
    , compactInfo  :: CompactInfo
    }

instance Eq NodeInfo where
    NodeInfo i c == NodeInfo i' c' = i == i' && c == c'

type Token = ByteString

stringpack :: String -> ByteString
stringpack = Char8.pack

stringunpack :: ByteString -> String
stringunpack = Char8.unpack

class Octets a where
    octets :: a -> [Word8]

instance Octets Word16 where
    octets w =
        [ fromIntegral (w `shiftR` 8)
        , fromIntegral w
        ]

instance Octets Word32 where
    octets w =
        [ fromIntegral (w `shiftR` 24)
        , fromIntegral (w `shiftR` 16)
        , fromIntegral (w `shiftR` 8)
        , fromIntegral w
        ]

instance Octets Word160 where
    octets (Word160 a1 a2 a3 a4 a5) = octets a1 ++ octets a2 ++ octets a3 ++ octets a4 ++ octets a5

instance Octets CompactInfo where
    octets (CompactInfo i p) = octets i ++ octets p

instance Octets NodeInfo where
    octets (NodeInfo nId nInfo) = octets nId ++ octets nInfo

fromOctets :: [Word8] -> Word32
fromOctets = foldl' accum 0
  where
    accum a o = (a `shiftL` 8) .|. fromIntegral o

bEncode :: (Octets a) => a -> BValue
bEncode = toBEncode . pack . octets

instance BEncode CompactInfo where
    toBEncode = bEncode
    fromBEncode = undefined

instance BEncode NodeInfo where
    toBEncode = bEncode
    fromBEncode = undefined

data Message
    = Query NodeID Message
    | Response NodeID Message
    | Ping
    | FindNode NodeID
--  | Nodes (Either NodeInfo [NodeInfo])
    | Nodes [NodeInfo]
    | AskPeers InfoHash
    | PeersFound Token Message
    | Values [CompactInfo]
    | AnnouncePeer InfoHash Port Token Bool -- last arg is implied_port
    | Error { errCode :: Integer, errMsg :: String }

instance Show Message where
    show (Error code msg) = "Error{code: " ++ show code
                                    ++ ", msg: " ++ show msg ++ "}"

    show (Query nid msg) = "Query<from: " ++ show nid ++ ">{"
                                        ++ show msg ++ "}"
    show Ping = "Ping"
    show _ = ""

instance Eq Message where
    Query i xs == Query i' xs' = i == i' && xs == xs'
    Response i xs == Response i' xs' = i == i' && xs == xs'
    Ping == Ping = True
    FindNode a == FindNode b = a == b
    Nodes a == Nodes b = a == b
    AskPeers a == AskPeers b = a == b
    PeersFound t m == PeersFound t' m' = t == t' && m == m'
    Values a == Values b = a == b
    AnnouncePeer i p t b == AnnouncePeer i' p' t' b'
        = i == i' && p == p' && t == t' && b == b'
    Error e m == Error e' m' = e == e' && m == m'
    _ == _ = False

bd :: String -> String -> BDictMap BValue
bd a b = singleton (stringpack a) (BString $ stringpack b)

bs_y :: ByteString
bs_y = stringpack "y"

bs_e :: ByteString
bs_e = stringpack "e"

bs_q :: ByteString
bs_q = stringpack "q"

bs_a :: ByteString
bs_a = stringpack "a"

bs_t :: ByteString
bs_t = stringpack "t"

bs_id :: ByteString
bs_id = stringpack "id"

msgName :: Message -> String
msgName  Ping                  = "ping"
msgName (FindNode _          ) = "find_node"
msgName (AskPeers _          ) = "get_peers"
msgName (AnnouncePeer _ _ _ _) = "announce_peer"
msgName _                      = undefined

msgToBDictMap :: Message -> BDictMap BValue
msgToBDictMap (Error code msg) = bd "y" "e"
    `union` singleton (stringpack "e") (BList [BInteger code, BString (stringpack msg)])

msgToBDictMap (Query queryNodeId m) = bd "y" "q"
    `union` bd "q" (msgName m)
    `union` singleton (stringpack "a")
           (BDict (singleton bs_id (bEncode queryNodeId) `union` msgToBDictMap m))

msgToBDictMap (Response respondNodeId m) = bd "y" "r"
    `union` singleton (stringpack "r")
           (BDict $ singleton bs_id (bEncode respondNodeId) `union` msgToBDictMap m)

msgToBDictMap Ping = Nil

msgToBDictMap (FindNode i) = singleton (stringpack "target") (bEncode i)

msgToBDictMap (Nodes (n : [])) = singleton (stringpack "nodes") (toBEncode n)
msgToBDictMap (Nodes n       ) = singleton (stringpack "nodes") (toBEncode n)

msgToBDictMap (AskPeers i) = singleton (stringpack "info_hash") (bEncode i)

msgToBDictMap (PeersFound t (Nodes n)) =
    singleton (stringpack "token") (toBEncode t)
    `union` singleton (stringpack "values") (toBEncode n)

msgToBDictMap (PeersFound t (Values c)) =
    singleton (stringpack "token") (toBEncode t)
    `union` singleton (stringpack "values") (toBEncode c)

msgToBDictMap (AnnouncePeer infohash portnum token implied_port) =
    singleton (stringpack "info_hash") (bEncode infohash)
    `union` singleton (stringpack "implied_port") (toBEncode implied_port)
    `union` singleton (stringpack "port") (toBEncode portnum)
    `union` singleton (stringpack "token") (toBEncode token)

msgToBDictMap _ = undefined

{-
 - >>> sorted([b'e', b'y', b't', b'q', b'r', b'a'])
 - [b'a', b'e', b'q', b'r', b't', b'y']
 - a e q r t y
 -}

parseNodeID :: ByteString -> NodeID
parseNodeID bs = Word160 a b c d e
    where bytes = unpack bs
          a = fromOctets $ take 32 bytes
          b = fromOctets $ take 32 (drop 32 bytes)
          c = fromOctets $ take 32 (drop 64 bytes)
          d = fromOctets $ take 32 (drop 96 bytes)
          e = fromOctets $ take 32 (drop 128 bytes)

bDictMapToMsg :: BDictMap BValue -> Message
bDictMapToMsg (Cons a (BDict (Cons i (BString nid) Nil))
                (Cons _ (BString qval)
                    (Cons y (BString yval) Nil)))
    |  y == bs_y
    && yval == bs_q
    && qval == stringpack "ping"
    && a == bs_a
    && i == bs_id
    = Query (parseNodeID nid) Ping

bDictMapToMsg (Cons
                e
                (BList ((BInteger code) : (BString msg) : []))
                (Cons y (BString yval) Nil))
    | e == bs_e && y == bs_y && yval == bs_e = Error code (stringunpack msg)

bDictMapToMsg _ = undefined

data KPacket = KPacket
    { transactionId :: ByteString
    , message       :: Message
    }

instance Show KPacket where
    show (KPacket t m) = "KPacket{t:" ++ show t ++ " " ++ show m ++ "}"

instance BEncode KPacket where
    toBEncode (KPacket { transactionId = t, message = m }) =
        BDict $ singleton (stringpack "t") (BString t) `union` (msgToBDictMap m)

    fromBEncode (BDict (Cons a meat
                            (Cons q qval
                                (Cons t (BString tid)
                                    (Cons y yval Nil)))))
        |  t == bs_t
        && q == bs_q = Right $ KPacket tid (bDictMapToMsg xs)
            where xs = singleton a meat
                        `union` singleton q qval
                        `union` singleton y yval

    fromBEncode (BDict (Cons a meat
                            (Cons t (BString tid)
                                (Cons y yval Nil))))
        | t == stringpack "t" = Right $ KPacket tid (bDictMapToMsg xs)
            where xs = singleton a meat `union` singleton y yval

    fromBEncode _ = decodingError "- this doesn't look like a KRPC message"

instance Eq KPacket where
    KPacket tid msg == KPacket tid' msg' = tid == tid' && msg == msg'
