import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework (defaultMain, testGroup, Test)

import Data.List (nub)
import Data.Word (Word32, Word16, Word8)
import qualified Data.Set as Set

import Data.ByteString (pack)

import Data.BEncode
import Data.BEncode.BDict hiding (map)

import Mainline.Bucket
import Network.Octets (Octets (..), fromByteString)
import Network.KRPC (KPacket (..), parseNodes)
import Network.KRPC.Helpers (extendListWith, stringpack, bd)
import Network.KRPC.Types
    ( CompactInfo
    , NodeInfo
    , Message     (..)
    , QueryDat    (..)
    , ResponseDat (..)
    )
import Network.KRPC.InternalConstants
    ( bs_a
    , bs_r
    , bs_t
    , bs_id
    )

import Network.Transport.Internal (decodeWord16, decodeWord32)
import TestBinaryTrie
    ( trieHasInsertedKey
    , deletedNotInTrie
    , closestReturnsSomething
    , closestIsXorClosest
    )

--import Debug.Trace

bucketsize :: Bucket a -> Int
bucketsize (Split a b) = bucketsize a + bucketsize b
bucketsize b = Set.size $ bucketData b

prop_fits :: Word32 -> Int -> Word32 -> Word32 -> Word32 -> Bool
prop_fits bid size bmin bmax value =
    fits value bucket == fits value (split bucket)
    where bucket = Bucket bid size bmin bmax Set.empty

prop_size :: Word32 -> [Word32] -> Bool
prop_size bid xs = bucketsize (foldr insert bucket (nub xs)) == len
    where len = length $ nub xs
          bucket = Bucket bid len minBound maxBound Set.empty

prop_bounds :: Word32 -> Int -> [Word32] -> Bool
prop_bounds bid bsize xs
    | bsize < 1 = True
    | otherwise = bounds filledBucket == bounds bucket
    where
        filledBucket = foldr insert bucket (nub xs)
        bounds (Split a b) = (fst $ bounds a, snd $ bounds b)
        bounds (Bucket _ _ min_ max_ _) = (min_, max_)
        bucket = Bucket bid bsize minBound maxBound Set.empty

prop_delete :: Word32 -> Int -> [Word32] -> Bool
prop_delete bid bsize xs
    | bsize < 1 = True
    | otherwise = bucketsize emptyBucket == 0
    where
        bucket :: Bucket Word32
        bucket = Bucket bid bsize minBound maxBound Set.empty

        filledBucket :: Bucket Word32
        filledBucket = foldr insert bucket (nub xs)

        emptyBucket :: Bucket Word32
        emptyBucket = foldr delete filledBucket xs


word16_bytestring_conversion :: [Word8] -> Bool
word16_bytestring_conversion bs =
    ((fromOctets padded) :: Word16) == (decodeWord16 $ pack padded)
    where
        padded = take 2 (extendListWith bs 0)

word16_bytestring_bijection :: [Word8] -> Bool
word16_bytestring_bijection b = padded == (octets . word16FromOctets) padded
    where
        padded = take 2 (extendListWith b 0)
        word16FromOctets = fromOctets :: [Word8] -> Word16

word32_bytestring_bijection :: [Word8] -> Bool
word32_bytestring_bijection b = padded == (octets . word16FromOctets) padded
    where
        padded = take 4 (extendListWith b 0)
        word16FromOctets = fromOctets :: [Word8] -> Word32

word32_bytestring_conversion :: [Word8] -> Bool
word32_bytestring_conversion bs =
    ((fromOctets padded) :: Word32) == (decodeWord32 $ pack padded)
    where
        padded = take 4 (extendListWith bs 0)

integer_bytestring_bijection :: [Word8] -> Bool
integer_bytestring_bijection b = padded == (octets . word160FromOctets) padded
    where
        padded = take 20 (extendListWith b 0)
        word160FromOctets = fromOctets :: [Word8] -> Integer

compactInfo_bytestring_bijection :: [Word8] -> Bool
compactInfo_bytestring_bijection b = padded == (octets . word16FromOctets) padded
    where
        padded = take 6 (extendListWith b 0)
        word16FromOctets = fromOctets :: [Word8] -> CompactInfo

nodeInfo_bytestring_bijection :: [Word8] -> Bool
nodeInfo_bytestring_bijection b = padded == (octets . word16FromOctets) padded
    where
        padded = take 26 (extendListWith b 0)
        word16FromOctets = fromOctets :: [Word8] -> NodeInfo

fmt_decodeErr :: [Word8] -> Integer -> String -> Bool
fmt_decodeErr i code msg =
    fromBEncode bval == expected
    where
        bval =
            BDict $ (singleton (stringpack "t") (BString tid))
                `union` bd "y" "e"
                `union` singleton
                    (stringpack "e")
                    (BList [BInteger code, BString (stringpack msg)])

        expected =
            Right
                ( KPacket
                    tid
                    (Error code (stringpack msg))
                    Nothing
                )

        tid = pack i

fmt_decodePingQuery :: [Word8] -> [Word8] -> Bool
fmt_decodePingQuery i n = fromBEncode bval == expected
    where
        bval =
            BDict $ singleton bs_a (BDict (singleton bs_id (BString nid)))
                `union` (singleton bs_t (BString tid))
                `union` bd "q" "ping"
                `union` bd "y" "q"

        expected =
            ( Right
                ( KPacket
                    tid
                    (Query (fromByteString nid) Ping)
                    Nothing
                )
            )

        tid = pack i
        nid = pack n

fmt_decodePingResponse :: [Word8] -> [Word8] -> Bool
fmt_decodePingResponse i n = fromBEncode bval == expected
    where
        bval = BDict $ singleton bs_r (BDict (singleton bs_id (BString nid)))
            `union` (singleton bs_t (BString tid))
            `union` bd "y" "r"

        expected =
            Right
                ( KPacket
                    tid
                    (Response (fromByteString nid) Pong)
                    Nothing
                )

        tid = pack i
        nid = pack n

fmt_decodeFindNodeQuery :: [Word8] -> [Word8] -> [Word8] -> Bool
fmt_decodeFindNodeQuery i n t =
    fromBEncode bval == expected

    where
        bval =
            BDict $
                singleton bs_a
                    (BDict (singleton bs_id (BString nid)
                        `union` singleton (stringpack "target") (BString target)))
                `union` (singleton bs_t (BString tid))
                `union` bd "q" "find_node"
                `union` bd "y" "q"

        expected =
            Right
                ( KPacket
                    tid
                    ( Query
                        (fromByteString nid)
                        (FindNode (fromByteString target))
                    )
                    Nothing
                )

        tid = pack i
        nid = pack n
        target = pack t

fmt_decodeNodesResponse :: [Word8] -> [Word8] -> [Word8] -> Bool
fmt_decodeNodesResponse t i ns = fromBEncode bval == expected
    where
        tid = pack t
        nid = pack i
        bval = BDict $
            singleton bs_r (BDict $
                singleton bs_id (BString nid)
                `union` singleton (stringpack "nodes") (BString $ pack ns))
            `union` bd "y" "r"
            `union` singleton bs_t (BString tid)

        expected :: Result KPacket
        expected =
            Right
                ( KPacket
                    tid
                    ( Response
                        (fromByteString nid)
                        (Nodes $ mkNodes ns)
                    )
                    Nothing
                )

        mkNodes :: [Word8] -> [NodeInfo]
        mkNodes [] = []
        mkNodes xs = fromOctets (take 26 xs) : mkNodes (drop 26 xs)

fmt_decodeGetPeersQuery :: [Word8] -> [Word8] -> [Word8] -> Bool
fmt_decodeGetPeersQuery t i info = fromBEncode bval == expected
    where
        expected =
            Right
                ( KPacket
                    tid
                    (Query (fromByteString nid) (GetPeers $ fromOctets info))
                    Nothing
                )

        tid = pack t
        nid = pack i
        bval = BDict $
            singleton bs_a (BDict $
                singleton bs_id (BString nid)
                `union` singleton (stringpack "info_hash") (BString (pack info)))
            `union` bd "y" "q"
            `union` bd "q" "get_peers"
            `union` singleton bs_t (BString tid)

fmt_decodeFoundValuesResponse
    :: [Word8]
    -> [Word8]
    -> [Word8]
    -> [[Word8]]
    -> Bool
fmt_decodeFoundValuesResponse t i token values = fromBEncode bval == expected
    where
        tid = pack t
        bval = BDict $
                singleton bs_r (BDict $
                    singleton bs_id (BString $ pack i)
                    `union` singleton (stringpack "token") (BString $ pack token)
                    `union` singleton (stringpack "values") (BList $ map (BString . pack) values))
                `union` bd "y" "r"
                `union` singleton bs_t (BString tid)

        expected =
            Right
                ( KPacket
                    tid
                    ( Response (fromOctets i)
                        ( PeersFound
                            (pack token)
                            (mkValues values)
                        )
                    )
                    Nothing
                )

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
                `union` singleton (stringpack "nodes") (BString $ pack nodes)
                `union` singleton (stringpack "token") (BString $ pack token))
            `union` bd "y" "r"
            `union` singleton bs_t (BString tid)
          expected =
              ( Right
                  ( KPacket
                      tid
                      ( Response (fromOctets i)
                          ( NodesFound
                              (pack token)
                              (parseNodes $ pack nodes)
                          )
                      )
                      Nothing
                  )
              )

fmt_decodeAnnoucePeerQuery
    :: [Word8]
    -> [Word8]
    -> [Word8]
    -> [Word8]
    -> Integer
    -> Bool
    -> Integer
    -> Bool
fmt_decodeAnnoucePeerQuery
    t
    i
    token
    nfo
    pval
    impliedPortArg
    impliedPortVal = fromBEncode bval == expected
        where
            tid = pack t
            bval = BDict $
                singleton bs_a (BDict $
                    singleton bs_id (BString $ pack i)
                    `union` impliedPort
                    `union` singleton (stringpack "info_hash") (BString $ pack nfo)
                    `union` singleton (stringpack "port") (BInteger pval)
                    `union` singleton (stringpack "token") (BString $ pack token))
                `union` bd "q" "announce_peer"
                `union` bd "y" "q"
                `union` singleton bs_t (BString tid)

            expected =
                Right
                    ( KPacket
                        tid
                        ( Query (fromOctets i)
                            ( AnnouncePeer
                                (impliedPortArg == True && impliedPortVal /= 0)
                                (fromOctets nfo)
                                (fromIntegral pval)
                                (pack token)
                                Nothing
                            )
                        )
                        Nothing
                    )

            impliedPort =
                if impliedPortArg then
                    singleton (stringpack "implied_port") (BInteger impliedPortVal)
                else
                    empty

tests :: [Test]
tests =
    [ testGroup "QuickCheck Bucket"
        [ testProperty "Fits into bucket and split"              prop_fits
        , testProperty "Always store things in boundless bucket" prop_size
        , testProperty "Insert should return same sized bucket"  prop_bounds
        , testProperty "Remove everything should yield empty"    prop_delete
        ]
    , testGroup "Words Octets instance is reversable"
        [ testProperty "ByteString to Word16 is reversable"      word16_bytestring_bijection
        , testProperty "ByteString to Word32 is reversable"      word32_bytestring_bijection
        , testProperty "ByteString to Integer is reversable"     integer_bytestring_bijection
        , testProperty "ByteString to CompactInfo is reversable" compactInfo_bytestring_bijection
        , testProperty "ByteString to NodeInfo is reversable"    nodeInfo_bytestring_bijection
        , testProperty "Compare parsing Word16 from octets"      word16_bytestring_conversion
        , testProperty "Compare parsing Word32 from octets"      word32_bytestring_conversion
        ]
    , testGroup "KRPC Sanity"
        [ testProperty "decode Error Message"                    fmt_decodeErr
        , testProperty "decode Ping Query Message"               fmt_decodePingQuery
        , testProperty "decode Ping Response Message"            fmt_decodePingResponse
        , testProperty "decode FindNode Query Message"           fmt_decodeFindNodeQuery
        , testProperty "decode Nodes Response Message"           fmt_decodeNodesResponse
        , testProperty "decode GetPeers Query Message"           fmt_decodeGetPeersQuery
        , testProperty "decode PeersFound Values Message"        fmt_decodeFoundValuesResponse
        , testProperty "decode PeersFound Nodes Message"         fmt_decodeFoundNodesResponse
        , testProperty "decode Announce Peer Query"              fmt_decodeAnnoucePeerQuery
        ]
    , testGroup "Binary Particia Trie"
        [ testProperty "contains inserted key" trieHasInsertedKey
        , testProperty "delete works" deletedNotInTrie
        , testProperty "closest always returns value" closestReturnsSomething
        , testProperty "closest value is xor distance closest" closestIsXorClosest
        ]
    ]

main :: IO ()
main = do
    defaultMain tests
