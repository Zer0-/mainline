--TODO: Validate lengths on parsing
module KRPC where

import Data.Word
import Data.Bits
import Data.List (foldl', intercalate)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import Data.Digest.SHA1 (Word160 (Word160))
import Data.BEncode
import Data.BEncode.BDict hiding (map)

type NodeID = Word160

type InfoHash = Word160

type Port = Word16

data CompactInfo = CompactInfo
    { ip    :: Word32
    , port  :: Port
    } deriving Eq

data NodeInfo = NodeInfo
    { nodeId       :: NodeID
    , compactInfo  :: CompactInfo
    } deriving Eq

type Token = BS.ByteString

stringpack :: String -> BS.ByteString
stringpack = Char8.pack

stringunpack :: BS.ByteString -> String
stringunpack = Char8.unpack

extendListWith :: (Enum a) => [a] -> a -> [a]
extendListWith l a = l ++ [a, a..]

numFromOctets :: (Num a, Bits a) => [Word8] -> a
numFromOctets = foldl' accum 0
  where
    accum a o = (a `shiftL` 8) .|. fromIntegral o

class Octets a where
    octets :: a -> [Word8]
    fromOctets :: [Word8] -> a

instance Octets Word16 where
    octets w =
        [ fromIntegral (w `shiftR` 8)
        , fromIntegral w
        ]
    fromOctets = numFromOctets . (take 2) . (`extendListWith` 0)

instance Octets Word32 where
    octets w =
        [ fromIntegral (w `shiftR` 24)
        , fromIntegral (w `shiftR` 16)
        , fromIntegral (w `shiftR` 8)
        , fromIntegral w
        ]
    fromOctets = numFromOctets . (take 4) . (`extendListWith` 0)

instance Octets Word160 where
    octets (Word160 a1 a2 a3 a4 a5)
        = octets a1 ++ octets a2 ++ octets a3 ++ octets a4 ++ octets a5

    fromOctets bytes = Word160 a b c d e
        where a = fromOctets $ take 4 bytes
              b = fromOctets $ take 4 (drop 4 bytes)
              c = fromOctets $ take 4 (drop 8 bytes)
              d = fromOctets $ take 4 (drop 12 bytes)
              e = fromOctets $ take 4 (drop 16 bytes)

instance Octets CompactInfo where
    octets (CompactInfo i p) = octets i ++ octets p

    fromOctets bytes = CompactInfo (fromOctets $ take 32 bytes)
                            (fromOctets $ take 16 (drop 32 bytes))

instance Octets NodeInfo where
    octets (NodeInfo nId nInfo) = octets nId ++ octets nInfo

    fromOctets bytes = NodeInfo (fromOctets $ take 160 bytes)
                            (fromOctets $ drop 160 bytes)

octToByteString :: (Octets a) => a -> BS.ByteString
octToByteString = BS.pack . octets

octToString :: (Octets a) => a -> String
octToString = show . octToByteString

bEncode :: (Octets a) => a -> BValue
bEncode = toBEncode . octToByteString

instance BEncode CompactInfo where
    toBEncode = bEncode
    fromBEncode = undefined

instance Show CompactInfo where
    show (CompactInfo i p)
        = "CompactInfo<ip: " ++ show i
        ++ " port: " ++ show p ++ ">"

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
    | Error { errCode :: Integer, errMsg :: String }
    deriving Eq

instance Show Message where
    show (Error code msg) = "Error{code: " ++ show code
                                    ++ ", msg: " ++ show msg ++ "}"

    show (Query nid msg) = "Query<from: " ++ octToString nid ++ ">{"
                                        ++ show msg ++ "}"
    show (Response nid msg) = "Response<from: " ++ octToString nid ++ ">{"
                                        ++ show msg ++ "}"
    show (FindNode nid) = "FindNode<" ++ octToString nid ++ ">"
    show (Nodes ns) = "Nodes[" ++ intercalate "," (map octToString ns) ++ "]"
    show (PeersFound t msg) = "PeersFound{token: " ++ show t ++ " " ++ show msg ++ "}"
    show (Values v) = "Values" ++ show v
    show Ping = "Ping"
    show _ = ""

bd :: String -> String -> BDictMap BValue
bd a b = singleton (stringpack a) (BString $ stringpack b)

bs_y :: BS.ByteString
bs_y = stringpack "y"

bs_e :: BS.ByteString
bs_e = stringpack "e"

bs_q :: BS.ByteString
bs_q = stringpack "q"

bs_a :: BS.ByteString
bs_a = stringpack "a"

bs_r :: BS.ByteString
bs_r = stringpack "r"

bs_t :: BS.ByteString
bs_t = stringpack "t"

bs_id :: BS.ByteString
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

msgToBDictMap (Nodes ns) = singleton (stringpack "nodes") (toBEncode nodes)
    where nodes = BS.concat $ map octToByteString ns

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

fromByteString :: (Octets a) => BS.ByteString -> a
fromByteString = fromOctets . BS.unpack

parseNodes :: BS.ByteString -> Message
parseNodes = Nodes . (map fromByteString) . splitBytes
    where splitBytes :: BS.ByteString -> [BS.ByteString]
          splitBytes b
              | BS.null b = []
              | otherwise = BS.take 166 b : splitBytes (BS.drop 166 b)

bDictMapToMsg :: BDictMap BValue -> Either String Message

--Ping Query
bDictMapToMsg (Cons a (BDict (Cons i (BString nid) Nil))
                (Cons _ (BString qval)
                    (Cons _ (BString yval) Nil)))
    |  yval == bs_q
    && qval == stringpack "ping"
    && a == bs_a
    && i == bs_id
    = Right $ Query (fromByteString nid) Ping

--Ping Response
bDictMapToMsg (Cons r (BDict (Cons i (BString nid) Nil))
                (Cons _ (BString yval) Nil))
    |  yval == bs_r
    && r == bs_r
    && i == bs_id
    = Right $ Response (fromByteString nid) Ping

--FindNode Query
bDictMapToMsg (Cons a (BDict (Cons i (BString nid) (Cons t (BString tval) Nil)))
                (Cons _ (BString qval)
                    (Cons _ (BString yval) Nil)))
    |  yval == bs_q
    && qval == stringpack "find_node"
    && t == stringpack "target"
    && a == bs_a
    && i == bs_id
    = Right $ Query (fromByteString nid) (FindNode (fromByteString tval))

--Nodes Response
bDictMapToMsg (Cons r (BDict (Cons i (BString nid) (Cons n (BString nodes) Nil)))
                (Cons _ (BString yval) Nil))
    |  yval == bs_r
    && r == bs_r
    && i == bs_id
    && n == stringpack "nodes"
    = Right $ Response (fromByteString nid) (parseNodes nodes)

--AskPeers Query
bDictMapToMsg (Cons a (BDict (Cons i (BString nid)
                             (Cons n (BString info) Nil)))
              (Cons _ (BString qval)
              (Cons _ (BString yval) Nil)))
    |  yval == bs_q
    && a == bs_a
    && i == bs_id
    && n == stringpack "info_hash"
    && qval == stringpack "get_peers"
    = Right $ Query (fromByteString nid) (AskPeers $ fromByteString info)

--PeersFound Values Response
bDictMapToMsg (Cons r (BDict
                      (Cons i (BString nid)
                      (Cons t (BString token)
                      (Cons v (BList values) Nil))))
                  (Cons _ (BString yval) Nil))
    | yval == bs_r
        && r == bs_r
        && i == bs_id
        && t == stringpack "token"
        && v == stringpack "values"
        && isListOfBS values
    = Right $ Response (fromByteString nid) (PeersFound token (parseValues values))
        where parseValues = Values . (map bsToCompactInfo)
              bsToCompactInfo (BString n') = fromByteString n'
              bsToCompactInfo _ = undefined
              isListOfBS [] = True
              isListOfBS ((BString _) : _) = True
              isListOfBS _ = False

--PeersFound Nodes Response
bDictMapToMsg (Cons r (BDict
                      (Cons i (BString nid)
                      (Cons n (BString nodes)
                      (Cons t (BString token) Nil))))
                  (Cons _ (BString yval) Nil))
    |  yval == bs_r
        && r == bs_r
        && i == bs_id
        && t == stringpack "token"
        && n == stringpack "nodes"
    = Right $ Response (fromByteString nid) (PeersFound token (parseNodes nodes))

--Announce Peers Query (implied port ignored)
bDictMapToMsg (Cons a (BDict
                      (Cons i (BString nid)
                      (Cons n (BString nfo)
                      (Cons p (BString pval)
                      (Cons t (BString token) Nil)))))
                  (Cons _ (BString qval)
                  (Cons _ (BString yval) Nil)))
    |  yval == bs_q
        && qval == stringpack "announce_peer"
        && a == bs_a
        && n == stringpack "info_hash"
        && p == stringpack "port"
        && i == bs_id
        && t == stringpack "token"
    = Right $ Query (fromByteString nid) $
        AnnouncePeer (fromByteString nfo)
                     (fromByteString pval)
                     token
                     False

--Announce Peers Query
bDictMapToMsg (Cons a (BDict
                      (Cons i (BString nid)
                      (Cons im (BInteger impliedPort)
                      (Cons n (BString nfo)
                      (Cons p (BString pval)
                      (Cons t (BString token) Nil))))))
                  (Cons _ (BString qval)
                  (Cons _ (BString yval) Nil)))
    |  yval == bs_q
        && qval == stringpack "announce_peer"
        && a == bs_a
        && im == stringpack "implied_port"
        && n == stringpack "info_hash"
        && p == stringpack "port"
        && i == bs_id
        && t == stringpack "token"
    = Right $ Query (fromByteString nid) $
        AnnouncePeer (fromByteString nfo)
                     (fromByteString pval)
                     token
                     (impliedPort == 1)

--Error Message
bDictMapToMsg (Cons e
                (BList ((BInteger code) : (BString msg) : []))
                (Cons y (BString yval) Nil))
    | e == bs_e && y == bs_y && yval == bs_e = Right $ Error code (stringunpack msg)

bDictMapToMsg _ = Left "KRPC message decoding error"

data KPacket = KPacket
    { transactionId :: BS.ByteString
    , message       :: Message
    } deriving Eq

instance Show KPacket where
    show (KPacket t m) = "KPacket{t:" ++ show t ++ " " ++ show m ++ "}"

instance BEncode KPacket where
    toBEncode (KPacket { transactionId = t, message = m }) =
        BDict $ singleton (stringpack "t") (BString t) `union` (msgToBDictMap m)

    fromBEncode (BDict (Cons a meat
                       (Cons q qval
                       (Cons t (BString tid)
                       (Cons y yval Nil)))))
        |  t == bs_t && q == bs_q =
            case bDictMapToMsg xs of
                Left  e -> Left e
                Right x -> Right $ KPacket tid x
            where xs = singleton a meat
                    `union` singleton q qval
                    `union` singleton y yval


    fromBEncode (BDict (Cons a meat
                       (Cons t (BString tid)
                       (Cons y yval Nil))))
        |  t == bs_t && y == bs_y  =
            case bDictMapToMsg xs of
                Left  e -> Left e
                Right x -> Right $ KPacket tid x
            where xs = singleton a meat `union` singleton y yval

    fromBEncode _ = decodingError "- this doesn't look like a KRPC message"
