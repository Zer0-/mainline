module Network.KRPC
    ( KPacket (..)
    , parseNodes
    ) where

import Data.BEncode (BValue (..), BEncode (..))
import Data.BEncode.BDict (BDictMap (..), singleton, empty, union)
import qualified Data.ByteString as BS
import Text.Parsec.Prim (runP, Parsec, tokenPrim)
import Text.Parsec.Pos (incSourceColumn)
import Text.Parsec.Combinator (between)

import Network.Octets (fromByteString, octToByteString)
import Network.KRPC.Types
    ( Message     (..)
    , QueryDat    (..)
    , ResponseDat (..)
    , NodeInfo
    , NodeID
    , bEncode
    )
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

    fromBEncode bvalue
        = either
            (Left . show)
            (Right . id)
            (runP kparser () "Inbound" (scanner bvalue))

    -- Parse query KPacket
    {-
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
    -}


    -- Parse response KPacket
    {-
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
    -}

    -- Parse response KPacket containing version key
    {-
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
    -}

    -- Parse response KPacket containing ip key
    -- (returned by certain bootstrap nodes)
    {-
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
    -}




msgToBDictMap :: Message -> BDictMap BValue
msgToBDictMap (Error { errCode = code, errMsg = msg}) =
    bd "y" "e" `union`
    singleton (stringpack "e") (BList errors)
    where
        errors = [BInteger code, BString msg]

msgToBDictMap (Query queryNodeId q) =
    bd "y" "q"           `union`
    bd "q" (queryName q) `union`
    singleton (stringpack "a") (BDict queryPayload)
    where
        queryPayload = singleton bs_id (bEncode queryNodeId) `union`
            queryToBDictMap q

msgToBDictMap (Response respondNodeId r) =
    bd "y" "r" `union`
    singleton
        (stringpack "r")
        ( BDict $
            singleton bs_id (bEncode respondNodeId) `union`
            responseToBDictMap r
        )

queryToBDictMap :: QueryDat -> BDictMap BValue

queryToBDictMap (Ping) = Nil

queryToBDictMap (FindNode i) = singleton (stringpack "target") (bEncode i)

queryToBDictMap (GetPeers i) = singleton (stringpack "info_hash") (bEncode i)

queryToBDictMap (AnnouncePeer implied_port infohash portnum token) =
    singleton
        (stringpack "implied_port")
        (toBEncode $ fromEnum implied_port) `union`
    singleton (stringpack "info_hash")    (bEncode infohash)       `union`
    singleton (stringpack "port")         (toBEncode portnum)      `union`
    singleton (stringpack "token")        (toBEncode token)


responseToBDictMap :: ResponseDat -> BDictMap BValue

responseToBDictMap Pong = Nil

responseToBDictMap (Nodes ns) =
    singleton (stringpack "nodes") (toBEncode $ concatNodes ns)


responseToBDictMap (PeersFound t infos) =
    singleton (stringpack "token")  (toBEncode t) `union`
    singleton (stringpack "values") (toBEncode infos)


responseToBDictMap (NodesFound t ns) =
    singleton (stringpack "token")  (toBEncode t) `union`
    singleton (stringpack "nodes")  (toBEncode $ concatNodes ns)


concatNodes :: [NodeInfo] -> BS.ByteString
concatNodes = BS.concat . map octToByteString



queryName :: QueryDat -> String
queryName  Ping                  = "ping"
queryName (FindNode _          ) = "find_node"
queryName (GetPeers _          ) = "get_peers"
queryName (AnnouncePeer _ _ _ _) = "announce_peer"



parseNodes :: BS.ByteString -> [NodeInfo]
parseNodes = (map fromByteString) . splitBytes
    where splitBytes :: BS.ByteString -> [BS.ByteString]
          splitBytes b
              | BS.null b = []
              | otherwise =
                  BS.take 26 b : splitBytes (BS.drop 26 b)



type Parser a = Parsec [BVal] () a


data BVal
    = Bs
    | Be
    | BBs BS.ByteString
    | BInt Integer
    | Li
    | Le
    deriving (Eq, Show)


kparser :: Parser KPacket
kparser = withObject $ do
    isBs bs_a
    withObject $ do
        isBs bs_id
        nodeid <- parseNodeid
        return undefined


parseNodeid :: Parser NodeID
parseNodeid = satisfy test
    where
        test (BBs nodeid) = Just $ fromByteString nodeid
        test _ = Nothing


withObject :: Parser a -> Parser a
withObject inner = between (isVal Bs) (isVal Be) inner



isVal :: BVal -> Parser ()
isVal x = satisfy (\v -> if x == v then Just () else Nothing)


isBs :: BS.ByteString -> Parser ()
isBs = isVal . BBs


satisfy :: (BVal -> Maybe a) -> Parser a
satisfy test = tokenPrim show updatePos test
    where
        updatePos pos _ _ = incSourceColumn pos 1



scanner :: BValue -> [BVal]
scanner (BInteger i) = [BInt i]
scanner (BString s) = [BBs s]
scanner (BDict d) = Bs : scannerB d ++ [Be]
    where
        scannerB :: BDictMap  BValue -> [BVal]
        scannerB Nil = []
        scannerB (Cons bs b d2) = BBs bs : scanner b ++ scannerB d2
scanner (BList l) = Li : foldl (++) [] (map scanner l) ++ [Le]
