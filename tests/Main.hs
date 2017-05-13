import Bucket
import KRPC

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework (defaultMain, testGroup, Test)

import Data.List (nub)
import Data.Word (Word32, Word16, Word8)
import qualified Data.Map as Map

import Data.ByteString (pack)

import Data.BEncode
import Data.BEncode.BDict hiding (map)
import Data.Digest.SHA1 (Word160)

--import Debug.Trace

prop_fits :: Word32 -> Int -> Word32 -> Word32 -> Word32 -> Bool
prop_fits bid size bmin bmax value =
    fits value bucket == fits value (split bucket)
    where bucket = Bucket bid size bmin bmax Map.empty

prop_size :: Word32 -> [Word32] -> Bool
prop_size bid xs = bsize (foldr (\j b -> insert j j b) bucket (nub xs)) == len
    where len = length $ nub xs
          bucket = Bucket bid len minBound maxBound Map.empty
          bsize (Split a b) = bsize a + bsize b
          bsize b = Map.size $ bucketData b

word16_bytestring_bijection :: [Word8] -> Bool
word16_bytestring_bijection b = padded == (octets . word16FromOctets) b
    where
        padded = take 2 (extendListWith b 0)
        word16FromOctets = fromOctets :: [Word8] -> Word16

word32_bytestring_bijection :: [Word8] -> Bool
word32_bytestring_bijection b = padded == (octets . word16FromOctets) b
    where
        padded = take 4 (extendListWith b 0)
        word16FromOctets = fromOctets :: [Word8] -> Word32

word160_bytestring_bijection :: [Word8] -> Bool
word160_bytestring_bijection b = padded == (octets . word16FromOctets) b
    where
        padded = take 20 (extendListWith b 0)
        word16FromOctets = fromOctets :: [Word8] -> Word160

fmt_decodeErr :: [Word8] -> Integer -> String -> Bool
fmt_decodeErr i code msg = fromBEncode bval == (Right $ KPacket tid (Error code msg))
    where bval = BDict $ (singleton (stringpack "t") (BString tid))
                            `union` bd "y" "e"
                            `union` (singleton (stringpack "e") (BList [BInteger code, BString (stringpack msg)]))
          tid = pack i

fmt_decodePingQuery :: [Word8] -> [Word8] -> Bool
fmt_decodePingQuery i n = fromBEncode bval == (Right $ KPacket tid (Query (fromByteString nid) Ping))
    where bval = BDict $ singleton bs_a (BDict (singleton bs_id (BString nid)))
                            `union` (singleton bs_t (BString tid))
                            `union` bd "q" "ping"
                            `union` bd "y" "q"
          tid = pack i
          nid = pack n

fmt_decodePingResponse :: [Word8] -> [Word8] -> Bool
fmt_decodePingResponse i n = fromBEncode bval == (Right $ KPacket tid (Response (fromByteString nid) Ping))
    where bval = BDict $ singleton bs_r (BDict (singleton bs_id (BString nid)))
                            `union` (singleton bs_t (BString tid))
                            `union` bd "y" "r"
          tid = pack i
          nid = pack n

fmt_decodeFindNodeQuery :: [Word8] -> [Word8] -> [Word8] -> Bool
fmt_decodeFindNodeQuery i n t = fromBEncode bval == (Right $ KPacket tid (Query (fromByteString nid) (FindNode (fromByteString target))))
    where bval = BDict $
                    singleton bs_a
                        (BDict (singleton bs_id (BString nid)
                                    `union` singleton (stringpack "target") (BString target)))
                    `union` (singleton bs_t (BString tid))
                    `union` bd "q" "find_node"
                    `union` bd "y" "q"
          tid = pack i
          nid = pack n
          target = pack t

fmt_decodeNodesResponse :: [Word8] -> [Word8] -> [Word8] -> Bool
fmt_decodeNodesResponse t i ns
    = fromBEncode bval == expected
    where tid = pack t
          nid = pack i
          bval = BDict $
            singleton bs_r (BDict $
                singleton bs_id (BString nid)
                `union` singleton (stringpack "nodes") (BString $ pack ns))
            `union` bd "y" "r"
            `union` singleton bs_t (BString tid)
          expected :: Result KPacket
          expected = (Right $
                       KPacket tid $
                           Response
                               (fromByteString nid)
                               (Nodes $ mkNodes ns))
          mkNodes :: [Word8] -> [NodeInfo]
          mkNodes [] = []
          mkNodes xs = fromOctets (take 166 xs) : mkNodes (drop 166 xs)

fmt_decodeAskPeersQuery :: [Word8] -> [Word8] -> [Word8] -> Bool
fmt_decodeAskPeersQuery t i info
    = fromBEncode bval == (Right $
                          KPacket tid $ Query (fromByteString nid) (AskPeers $ fromOctets info))
    where tid = pack t
          nid = pack i
          bval = BDict $
            singleton bs_a (BDict $
                singleton bs_id (BString nid)
                `union` singleton (stringpack "info_hash") (BString (pack info)))
            `union` bd "y" "q"
            `union` bd "q" "get_peers"
            `union` singleton bs_t (BString tid)

fmt_decodeFoundValuesResponse :: [Word8] -> [Word8] -> [Word8] -> [[Word8]] -> Bool
fmt_decodeFoundValuesResponse t i token values
    = fromBEncode bval == expected
    where tid = pack t
          bval = BDict $
            singleton bs_r (BDict $
                singleton bs_id (BString $ pack i)
                `union` singleton (stringpack "token") (BString $ pack token)
                `union` singleton (stringpack "values") (BList $ map (BString . pack) values))
            `union` bd "y" "r"
            `union` singleton bs_t (BString tid)
          expected = (Right $ KPacket tid $
                           Response (fromOctets i) $
                           PeersFound (pack token) (Values $ mkValues values))
          mkValues :: [[Word8]] -> [CompactInfo]
          mkValues [] = []
          mkValues (x : xs) = fromOctets x : mkValues xs

fmt_decodeFoundNodesResponse :: [Word8] -> [Word8] -> [Word8] -> [Word8] -> Bool
fmt_decodeFoundNodesResponse t i token nodes
    = fromBEncode bval == expected
    where tid = pack t
          bval = BDict $
            singleton bs_r (BDict $
                singleton bs_id (BString $ pack i)
                `union` singleton (stringpack "token") (BString $ pack token)
                `union` singleton (stringpack "nodes") (BString $ pack nodes))
            `union` bd "y" "r"
            `union` singleton bs_t (BString tid)
          expected = (Right $ KPacket tid $
                           Response (fromOctets i) $
                           PeersFound (pack token) (parseNodes $ pack nodes))

fmt_decodeAnnoucePeerQuery
    :: [Word8]
    -> [Word8]
    -> [Word8]
    -> [Word8]
    -> [Word8]
    -> Bool
    -> Integer
    -> Bool
fmt_decodeAnnoucePeerQuery t i token nfo pval impliedPortArg impliedPortVal
    = fromBEncode bval == expected
    where tid = pack t
          bval = BDict $
            singleton bs_a (BDict $
                singleton bs_id (BString $ pack i)
                `union` impliedPort
                `union` singleton (stringpack "info_hash") (BString $ pack nfo)
                `union` singleton (stringpack "port") (BString $ pack pval)
                `union` singleton (stringpack "token") (BString $ pack token))
            `union` bd "q" "announce_peer"
            `union` bd "y" "q"
            `union` singleton bs_t (BString tid)
          expected = (Right $ KPacket tid $
                           Query (fromOctets i) $
                               AnnouncePeer (fromOctets nfo)
                                            (fromOctets pval)
                                            (pack token)
                                            (impliedPortArg == True && impliedPortVal == 1))
          impliedPort = if impliedPortArg
                            then singleton (stringpack "implied_port") (BInteger impliedPortVal)
                            else empty

tests :: [Test]
tests =
    [ testGroup "QuickCheck Bucket"
        [ testProperty "Fits into bucket and split"              prop_fits
        , testProperty "Always store things in boundless bucket" prop_size
        ]
    , testGroup "Words Octets instance is reversable"
        [ testProperty "ByteString to Word16 is reversable"      word16_bytestring_bijection
        , testProperty "ByteString to Word32 is reversable"      word32_bytestring_bijection
        , testProperty "ByteString to Word160 is reversable"     word160_bytestring_bijection
        ]
    , testGroup "KRPC Sanity"
        [ testProperty "decode Error Message"                    fmt_decodeErr
        , testProperty "decode Ping Query Message"               fmt_decodePingQuery
        , testProperty "decode Ping Response Message"            fmt_decodePingResponse
        , testProperty "decode FindNode Query Message"           fmt_decodeFindNodeQuery
        , testProperty "decode Nodes Response Message"           fmt_decodeNodesResponse
        , testProperty "decode AskPeers Query Message"           fmt_decodeAskPeersQuery
        , testProperty "decode PeersFound Values Message"        fmt_decodeFoundValuesResponse
        , testProperty "decode PeersFound Nodes Message"         fmt_decodeFoundNodesResponse
        , testProperty "decode Announce Peer Query"              fmt_decodeAnnoucePeerQuery
        ]
    ]

main :: IO ()
main = do
    defaultMain tests
