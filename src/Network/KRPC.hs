module Network.KRPC
    ( KPacket (..)
    , parseNodes
    ) where

import Data.BEncode (BValue (..), BEncode (..), decodingError)
import Data.BEncode.BDict (BDictMap (..), singleton, empty, union)
import qualified Data.ByteString as BS

import Network.Octets (fromByteString, octToByteString)
import Network.KRPC.Types (Message (..), bEncode)
import Network.KRPC.Helpers
    ( stringpack
    , bd
    )
import Network.KRPC.InternalConstants
    ( bs_y
    , bs_e
    , bs_q
    , bs_a
    , bs_r
    , bs_t
    , bs_id
    , bs_v
    , bs_ip
    )
import Network.KRPC.Helpers (hexify)

--
-- TODO: Quickcheck on arbitrary KPackets
data KPacket = KPacket
    { transactionId :: BS.ByteString
    , message       :: Message
    , version       :: Maybe BS.ByteString
    } deriving Eq


instance Show KPacket where
    show (KPacket t m mv) =
        "<KPacket t:" ++ hexify (BS.unpack t)
        ++ " msg: " ++ show m
        ++ maybe "" (\v -> " v: " ++ show v) mv
        ++ ">"


instance BEncode KPacket where
    toBEncode (KPacket { transactionId = t, message = m, version = mv }) =
        BDict $
            singleton bs_t (BString t) `union`
            maybe empty (\v -> singleton bs_v (BString v)) mv `union`
            (msgToBDictMap m)

    -- Parse query KPacket
    fromBEncode (BDict (Cons a meat
                       (Cons q qval
                       (Cons t (BString tid)
                       (Cons y yval Nil)))))
        |  t == bs_t && q == bs_q =
            case bDictMapToMsg xs of
                Left  e -> Left e
                Right x -> Right $ KPacket tid x Nothing
            where xs = singleton a meat
                    `union` singleton q qval
                    `union` singleton y yval


    -- Parse response KPacket
    fromBEncode (BDict (Cons a meat
                       (Cons t (BString tid)
                       (Cons y yval Nil))))
        |  t == bs_t && y == bs_y  =
            case bDictMapToMsg xs of
                Left  e -> Left e
                Right x -> Right $ KPacket tid x Nothing
            where
                xs = singleton a meat `union`
                    singleton y yval

    -- Parse response KPacket containing version key
    fromBEncode
        ( BDict
            ( Cons a meat
                ( Cons t (BString tid)
                    ( Cons v (BString vs)
                        ( Cons y yval Nil )
                    )
                )
            )
        )
        |  v == bs_v && t == bs_t && y == bs_y  =
            case bDictMapToMsg xs of
                Left  e -> Left e
                Right x -> Right $ KPacket tid x (Just vs)
            where
                xs = singleton a meat `union`
                    singleton y yval

    -- Parse response KPacket containing ip key
    -- (returned by certain bootstrap nodes)
    fromBEncode
        ( BDict
            ( Cons i _
                ( Cons a meat
                    ( Cons t (BString tid)
                        ( Cons y yval Nil )
                    )
                )
            )
        )
        |  i == bs_ip && t == bs_t && y == bs_y  =
            case bDictMapToMsg xs of
                Left  e -> Left e
                Right x -> Right $ KPacket tid x Nothing
            where
                xs = singleton a meat `union`
                    singleton y yval

    fromBEncode _ = decodingError "- this doesn't look like a KRPC message"




msgToBDictMap :: Message -> BDictMap BValue
msgToBDictMap (Error { errCode = code, errMsg = msg}) =
    bd "y" "e" `union`
    singleton (stringpack "e") (BList errors)
    where
        errors = [BInteger code, BString msg]

msgToBDictMap (Query queryNodeId m) =
    bd "y" "q"         `union`
    bd "q" (msgName m) `union`
    singleton (stringpack "a") (BDict queryPayload)
    where
        queryPayload = singleton bs_id (bEncode queryNodeId) `union`
            msgToBDictMap m

msgToBDictMap (Response respondNodeId m) =
    bd "y" "r" `union`
    singleton
        (stringpack "r")
        ( BDict $
            singleton bs_id (bEncode respondNodeId) `union`
            msgToBDictMap m
        )

msgToBDictMap Ping = Nil

msgToBDictMap (FindNode i) = singleton (stringpack "target") (bEncode i)

msgToBDictMap (Nodes ns) =
    singleton (stringpack "nodes") (toBEncode nodes)
    where
        nodes = BS.concat $ map octToByteString ns

msgToBDictMap (GetPeers i) = singleton (stringpack "info_hash") (bEncode i)

msgToBDictMap (PeersFound t (Nodes n)) =
    singleton (stringpack "token")  (toBEncode t) `union`
    singleton (stringpack "values") (toBEncode n)

msgToBDictMap (PeersFound t (Values c)) =
    singleton (stringpack "token")  (toBEncode t) `union`
    singleton (stringpack "values") (toBEncode c)

msgToBDictMap (AnnouncePeer infohash portnum token implied_port) =
    singleton (stringpack "info_hash")    (bEncode infohash)       `union`
    singleton (stringpack "implied_port") (toBEncode implied_port) `union`
    singleton (stringpack "port")         (toBEncode portnum)      `union`
    singleton (stringpack "token")        (toBEncode token)

msgToBDictMap _ = undefined




msgName :: Message -> String
msgName  Ping                  = "ping"
msgName (FindNode _          ) = "find_node"
msgName (GetPeers _          ) = "get_peers"
msgName (AnnouncePeer _ _ _ _) = "announce_peer"
msgName _                      = undefined




{-
 - This implementation is not ideal for several reasons:
 -      - 'q' and 'y' keys are never checked (perhaps too permissive)
 -      - NodeInfo/CompactInfo never checked for length, this can lead to
 -        us sending KPackets to someone else
 -}
bDictMapToMsg :: BDictMap BValue -> Either String Message

--Ping Query
bDictMapToMsg (Cons a (BDict (Cons i (BString nid) Nil))
              (Cons _ (BString qval)
              (Cons _ (BString yval) Nil)))
    |  yval == bs_q
    && qval == stringpack "ping"
    && a    == bs_a
    && i    == bs_id =
        Right $ Query (fromByteString nid) Ping

--Ping Response
bDictMapToMsg (Cons r (BDict (Cons i (BString nid) Nil))
              (Cons _ (BString yval) Nil))
    |  yval == bs_r
    && r    == bs_r
    && i    == bs_id =
        Right $ Response (fromByteString nid) Ping

--FindNode Query
bDictMapToMsg (Cons a (BDict (Cons i (BString nid)
                             (Cons t (BString tval) Nil)))
              (Cons _ (BString qval)
              (Cons _ (BString yval) Nil)))
    |  yval == bs_q
    && qval == stringpack "find_node"
    && t == stringpack "target"
    && a == bs_a
    && i == bs_id =
        Right $ Query (fromByteString nid) (FindNode (fromByteString tval))

--Nodes Response
bDictMapToMsg (Cons r (BDict (Cons i (BString nid)
                             (Cons n (BString nodes) Nil)))
              (Cons _ (BString yval) Nil))
    |  yval == bs_r
    && r    == bs_r
    && i    == bs_id
    && n    == stringpack "nodes" =
        Right $ Response (fromByteString nid) (parseNodes nodes)

--GetPeers Query
bDictMapToMsg (Cons a (BDict (Cons i (BString nid)
                             (Cons n (BString info) Nil)))
              (Cons _ (BString qval)
              (Cons _ (BString yval) Nil)))
    |  yval == bs_q
    && a    == bs_a
    && i    == bs_id
    && n    == stringpack "info_hash"
    && qval == stringpack "get_peers" =
        Right $ Query (fromByteString nid) (GetPeers $ fromByteString info)

--PeersFound Values Response
bDictMapToMsg (Cons r (BDict (Cons i (BString nid)
                             (Cons t (BString token)
                             (Cons v (BList values) Nil))))
              (Cons _ (BString yval) Nil))
    | yval == bs_r
    && r   == bs_r
    && i   == bs_id
    && t   == stringpack "token"
    && v   == stringpack "values"
    && isListOfBS values =
        Right $ Response (fromByteString nid) peersFound
        where
            peersFound = PeersFound token (parseValues values)
            parseValues = Values . (map bsToCompactInfo)
            bsToCompactInfo (BString n') = fromByteString n'
            bsToCompactInfo _ = undefined
            isListOfBS [] = True
            isListOfBS ((BString _) : _) = True
            isListOfBS _ = False

--PeersFound Nodes Response
bDictMapToMsg (Cons r (BDict (Cons i (BString nid)
                             (Cons n (BString nodes)
                             (Cons t (BString token) Nil))))
              (Cons _ (BString yval) Nil))
    |  yval == bs_r
    && r    == bs_r
    && i    == bs_id
    && t    == stringpack "token"
    && n    == stringpack "nodes" =
        Right $ Response (fromByteString nid) peersFound
        where
            peersFound = PeersFound token (parseNodes nodes)

--Announce Peers Query (implied port ignored)
bDictMapToMsg (Cons a (BDict (Cons i (BString nid)
                             (Cons n (BString nfo)
                             (Cons p (BString pval)
                             (Cons t (BString token) Nil)))))
                  (Cons _ (BString qval)
                  (Cons _ (BString yval) Nil)))
    |  yval == bs_q
    && qval == stringpack "announce_peer"
    && a    == bs_a
    && n    == stringpack "info_hash"
    && p    == stringpack "port"
    && i    == bs_id
    && t    == stringpack "token" =
        Right $ Query (fromByteString nid) $
            AnnouncePeer (fromByteString nfo) (fromByteString pval) token False

--Announce Peers Query
bDictMapToMsg (Cons a (BDict (Cons i  (BString nid)
                             (Cons im (BInteger impliedPort)
                             (Cons n  (BString nfo)
                             (Cons p  (BString pval)
                             (Cons t  (BString token) Nil))))))
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
bDictMapToMsg (Cons e (BList ((BInteger code) : (BString msg) : []))
              (Cons y (BString yval) Nil))
    |  e    == bs_e
    && y    == bs_y
    && yval == bs_e =
        Right $ Error code msg

bDictMapToMsg _ = Left "KRPC message decoding error"




parseNodes :: BS.ByteString -> Message
parseNodes = Nodes . (map fromByteString) . splitBytes
    where splitBytes :: BS.ByteString -> [BS.ByteString]
          splitBytes b
              | BS.null b = []
              | otherwise =
                  BS.take 26 b : splitBytes (BS.drop 26 b)


data BVal
    = Bs
    | Be
    | BBs BS.ByteString
    | BInt Integer
    | Li
    | Le


scanner :: BValue -> [BVal]
scanner (BInteger i) = [BInt i]
scanner (BString s) = [BBs s]
scanner (BDict d) = Bs : scannerB d ++ [Be]
    where
        scannerB :: BDictMap  BValue -> [BVal]
        scannerB Nil = []
        scannerB (Cons bs b d2) = BBs bs : scanner b ++ scannerB d2
scanner (BList l) = Li : foldl (++) [] (map scanner l) ++ [Le]
