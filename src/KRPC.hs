module KRPC where

import Data.BEncode
import Data.BEncode.BDict

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)

type NodeID = Int --Word160

type InfoHash = Int --Word160

type IP = Int

type Port = Int

type CompactInfo = (IP, Port)

type NodeInfo = (NodeID, CompactInfo)

type Token = [Int] -- [Word8]

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
    | AnnouncePeer Port Token Bool -- last arg is implied_port
    | Error { errCode :: Integer, errMsg :: String }

bd :: String -> String -> BDictMap BValue
bd a b = singleton (pack a) (BString $ pack b)

msgName :: Message -> String
msgName Ping = "ping"
msgName (FindNode _) = "find_node"
msgName (AskPeers _) = "get_peers"
msgName (AnnouncePeer _ _ _) = "announce_peer"
msgName _ = undefined

msgToBDictMap :: Message -> BDictMap BValue
msgToBDictMap (Error code msg) = bd "y" "e"
    `union` singleton (pack "e") (BList [BInteger code, BString (pack msg)])

msgToBDictMap (Query i m) = bd "y" "q"
    `union` bd "q" (msgName m)
    `union` singleton (pack "a")
           (BDict (singleton (pack "id") (toBEncode i) `union` msgToBDictMap m))

msgToBDictMap (Response i m) = bd "y" "r"
    `union` singleton (pack "r")
           (BDict $ singleton (pack "id") (toBEncode i) `union` msgToBDictMap m)

msgToBDictMap Ping = Nil

msgToBDictMap (FindNode n) = singleton (pack "target") (toBEncode n)

msgToBDictMap (Nodes (n : [])) = singleton (pack "nodes") (toBEncode n)
msgToBDictMap (Nodes n       ) = singleton (pack "nodes") (toBEncode n)

msgToBDictMap _ = undefined

data KPacket = KPacket
    { transactionId :: ByteString
    , message       :: Message
    }

instance BEncode KPacket where
    toBEncode (KPacket { transactionId = t, message = m }) =
        BDict $ singleton (pack "t") (BString t) `union` (msgToBDictMap m)

    fromBEncode _ = undefined

testKPacket :: KPacket
testKPacket = KPacket (pack "transaction") (Query 1337 Ping)
