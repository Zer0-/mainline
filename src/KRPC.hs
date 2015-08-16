module KRPC where

import Data.Word
import Data.Bits
import Data.ByteString (pack, ByteString)
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

data NodeInfo = NodeInfo
    { nodeId       :: NodeID
    , compactInfo  :: CompactInfo
    }

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
    octets (Word160 a1 a2 a3 a4 a5) = octets a1  ++ octets a2 ++ octets a3 ++ octets a4 ++ octets a5

instance Octets CompactInfo where
    octets (CompactInfo i p) = octets i ++ octets p

instance Octets NodeInfo where
    octets (NodeInfo nId nInfo) = octets nId ++ octets nInfo

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

bd :: String -> String -> BDictMap BValue
bd a b = singleton (stringpack a) (BString $ stringpack b)

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
           (BDict (singleton (stringpack "id") (bEncode queryNodeId) `union` msgToBDictMap m))

msgToBDictMap (Response respondNodeId m) = bd "y" "r"
    `union` singleton (stringpack "r")
           (BDict $ singleton (stringpack "id") (bEncode respondNodeId) `union` msgToBDictMap m)

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

bs_y :: ByteString
bs_y = stringpack "y"

bs_e :: ByteString
bs_e = stringpack "e"

bDictMapToMsg :: BDictMap BValue -> Message
bDictMapToMsg (Cons
                chrErr
                (BList ((BInteger code) : (BString msg) : []))
                Nil)
    | chrErr == bs_e = Error code (stringunpack msg)

bDictMapToMsg (Cons chrMsgTypeKey (BString chrMsgTypeVal) xs)
    | chrMsgTypeKey == bs_y && chrMsgTypeVal == bs_e = bDictMapToMsg xs

bDictMapToMsg _ = undefined

data KPacket = KPacket
    { transactionId :: ByteString
    , message       :: Message
    }

instance BEncode KPacket where
    toBEncode (KPacket { transactionId = t, message = m }) =
        BDict $ singleton (stringpack "t") (BString t) `union` (msgToBDictMap m)

    fromBEncode (BDict (Cons t (BString tid) xs))
        | t == stringpack "t" = Right $ KPacket tid (bDictMapToMsg xs)
    fromBEncode _ = decodingError "this doesn't look like a KRPC message"

{-
testKPacket :: KPacket
testKPacket = KPacket (stringpack "transaction") (Query (Word160 0 0 0 0 1337) Ping)
-}

testKPacket :: KPacket
testKPacket = KPacket (stringpack "transaction2") (
    Nodes [ NodeInfo (Word160 1 1 1 1 1337) (CompactInfo 192 8080)
          , NodeInfo (Word160 2 2 2 2 1338) (CompactInfo 168 9999)
          ]
    )
