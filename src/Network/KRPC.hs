module Network.KRPC
    ( KPacket (..)
    , parseNodes
    , scanner
    ) where

import Data.BEncode (BValue (..), BEncode (..))
import Data.BEncode.BDict (BDictMap (..), singleton, empty, union)
import qualified Data.ByteString as BS
import Text.Parsec.Prim (runP, Parsec, tokenPrim, (<|>), try, many)
import Text.Parsec.Pos (incSourceColumn)
import Text.Parsec.Combinator (between, option, optional)

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
        "<KPacket t:" ++ hexify t
        ++ " msg: " ++ show m
        ++ maybe "" (\v -> " v: " ++ show v) mv
        ++ ">"


instance BEncode KPacket where
    toBEncode (KPacket { transactionId = t, message = m, version = mv }) =
        BDict $
            singleton bs_t (BString t)
            `union` maybe empty (\v -> singleton bs_v (BString v)) mv
            `union` (msgToBDictMap m)

    fromBEncode bvalue
        = either
            (Left . show)
            Right
            (runP kparser () "Inbound" (scanner bvalue))



msgToBDictMap :: Message -> BDictMap BValue
msgToBDictMap (Error { errCode = code, errMsg = msg}) =
    bd "y" "e" `union`
    singleton (stringpack "e") (BList [BInteger code, BString msg])


msgToBDictMap (Query queryNodeId q) =
    bd "y" "q"           `union`
    bd "q" (queryName q) `union`
    singleton (stringpack "a") (BDict queryPayload)

    where
        queryPayload
            = singleton bs_id (bEncode queryNodeId)
            `union` queryToBDictMap q

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
queryToBDictMap (AnnouncePeer implied_port infohash portnum token _) =
    singleton
        (stringpack "implied_port")
        (toBEncode $ fromEnum implied_port)                `union`
    singleton (stringpack "info_hash") (bEncode infohash)  `union`
    singleton (stringpack "port")      (toBEncode portnum) `union`
    singleton (stringpack "token")     (toBEncode token)



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
queryName  Ping                    = "ping"
queryName (FindNode _          )   = "find_node"
queryName (GetPeers _          )   = "get_peers"
queryName (AnnouncePeer _ _ _ _ _) = "announce_peer"



parseNodes :: BS.ByteString -> [NodeInfo]
parseNodes = (map fromByteString) . splitBytes
    where splitBytes :: BS.ByteString -> [BS.ByteString]
          splitBytes b
              | BS.null b = []
              | otherwise =
                  BS.take 26 b : splitBytes (BS.drop 26 b)


type Parser a = Parsec [BVal] () a


data BVal
    = Ds
    | De
    | Bs BS.ByteString
    | BInt Integer
    | Li
    | Le
    deriving (Eq, Show)


kparser :: Parser KPacket
kparser = withObject $ do
    optional (isBs bs_ip >> parseBs)
    msg <- (qparser <|> rparser <|> eparser)

    case msg of
        (Query _ m) -> isBs bs_q >> (isBs $ stringpack $ queryName m)
        _ -> return ()

    isBs bs_t
    tid <- parseBs

    v <- option Nothing (isBs bs_v >> (Just <$> (try parseBs)))
    isBs bs_y

    case msg of
        (Query _ _) -> isBs bs_q
        (Response _ _) -> isBs bs_r
        (Error _ _) -> isBs bs_e

    return $ KPacket tid msg v


eparser :: Parser Message
eparser = isBs bs_e >> withList (Error <$> parseInt <*> parseBs)


rparser :: Parser Message
rparser = do
    isBs bs_r
    withObject $ do
        isBs bs_id
        nodeid <- parseNodeid
        rd <- rdatparser
        return $ Response nodeid rd


rdatparser :: Parser ResponseDat
rdatparser
    =   try peersFound
    <|> try nodesFound
    <|> (Nodes . parseNodes <$> nodes)
    <|> return Pong

    where
        nodes = do
            optional (isBs bs_ip >> parseBs)
            isBs (stringpack "nodes")
            ns <- parseBs
            --some clients put this "p" param here
            --seems to be a port value; purpose unknown
            optional (isBs (stringpack "p") >> parseInt)
            return ns

        peersFound = do
            optional (isBs bs_ip >> parseBs)
            optional (isBs (stringpack "nodes") >> parseBs)
            optional (isBs (stringpack "p") >> parseInt)

            token <- toke

            isBs (stringpack "values")
            cmptinfos <- withList $ many parseBs

            return $ PeersFound token (map fromByteString cmptinfos)

        nodesFound = do
            bs <- isBs (stringpack "nodes") >> parseBs

            optional (isBs (stringpack "p") >> parseInt)

            token <- toke

            return $ NodesFound token (parseNodes bs)

        toke = isBs (stringpack "token") >> parseBs




qparser :: Parser Message
qparser = do
    isBs bs_a
    withObject $ do
        isBs bs_id
        nodeid <- parseNodeid
        qd <- qdatparser
        return $ Query nodeid qd


qdatparser :: Parser QueryDat
qdatparser
    =   findNode
    <|> (try announcePeer)
    <|> getPeers
    <|> return Ping

    where
        findNode = do
            isBs (stringpack "target")
            nodeid <- try parseNodeid
            parseWant
            return $ FindNode nodeid

        getPeers = do
            isBs (stringpack "info_hash")
            nodeid <- try parseNodeid

            --noseed/scrape is for bep_0033 (not yet implemented)
            optional (isBs (stringpack "noseed") >> parseInt)
            optional (isBs (stringpack "scrape") >> parseInt)
            parseWant

            return $ GetPeers nodeid

        announcePeer = do
            flag <- option False impliedPort

            isBs (stringpack "info_hash")
            info_hash <- parseNodeid

            mname <- option Nothing (isBs (stringpack "name") >> (Just <$> parseBs))

            isBs (stringpack "port")
            port <- (fromIntegral <$> parseInt)

            optional (isBs (stringpack "seed") >> parseInt)

            isBs (stringpack "token")
            token <- parseBs

            return $ AnnouncePeer flag info_hash port token mname

        impliedPort
            = (/=) 0 <$> (isBs (stringpack "implied_port") >> parseInt)


parseInt :: Parser Integer
parseInt = satisfy test
    where
        test (BInt i) = Just i
        test _ = Nothing


parseBs :: Parser BS.ByteString
parseBs = satisfy test
    where
        test (Bs bs) = Just bs
        test _ = Nothing


parseNodeid :: Parser NodeID
parseNodeid = fromByteString <$> parseBs


-- This is for IPv6 support and is currently unimplemented
parseWant :: Parser ()
parseWant = optional (isBs (stringpack "want") >> (withList $ many parseBs))


withList :: Parser a -> Parser a
withList = wrappedBy Li Le


withObject :: Parser a -> Parser a
withObject = wrappedBy Ds De

wrappedBy :: BVal -> BVal -> Parser a -> Parser a
wrappedBy s e inner = between (isVal s) (isVal e) inner


isVal :: BVal -> Parser ()
isVal x = satisfy (\v -> if x == v then Just () else Nothing)


isBs :: BS.ByteString -> Parser ()
isBs = isVal . Bs


satisfy :: (BVal -> Maybe a) -> Parser a
satisfy test = tokenPrim show updatePos test
    where
        updatePos pos _ _ = incSourceColumn pos 1


scanner :: BValue -> [BVal]
scanner (BInteger i) = [BInt i]
scanner (BString s) = [Bs s]
scanner (BDict d) = Ds : scannerB d ++ [De]
    where
        scannerB :: BDictMap  BValue -> [BVal]
        scannerB Nil = []
        scannerB (Cons bs b d2) = Bs bs : scanner b ++ scannerB d2
scanner (BList l) = Li : (l >>= scanner) ++ [Le]
