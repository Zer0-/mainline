{-# LANGUAGE NamedFieldPuns #-}

module Architecture.Internal.Sub
    ( TSub (..)
    , Sub (..)
    , SubStates
    , updateSubscriptions
    , readSubscriptions
    ) where

import Data.Map (Map)
import Control.Monad (foldM)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Network.Socket
    ( Socket
    , socket
    , SocketType (Stream)
    , Family (AF_INET)
    , AddrInfo (addrFlags, addrFamily, addrSocketType, addrAddress)
    , AddrInfoFlag (AI_PASSIVE)
    , defaultProtocol
    , defaultHints
    , getAddrInfo
    , bind
    , listen
    , close
    , accept
    )
import Network.Socket.ByteString (recv)
import Data.Hashable
import qualified Data.ByteString as BS

import Network.KRPC.Types (Port)


data SubscriptionData msg
    = TCPDat
        { port :: Port
        , listenSocket :: Socket
        , connectedSocket :: Maybe Socket
        , handler :: (BS.ByteString -> msg)
        }


data TSub msg
    = TCP Port (BS.ByteString -> msg)

instance Hashable (TSub msg) where
    hashWithSalt s (TCP p _) = s `hashWithSalt` (0::Int) `hashWithSalt` p


type SubStates msg = Map Int (SubscriptionData msg)


newtype Sub msg = Sub [ TSub msg ]


updateSubscriptions :: SubStates msg -> Sub msg -> IO (SubStates msg)
updateSubscriptions substates (Sub tsubs) = do
    mapM_ unsub unloads
    foldM
        foldSubs
        (Map.empty :: SubStates msg)
        loads
    --foldM (\s (k, t) -> (sub t >>= \s_ -> return $ Map.insert k s_ s)) Map.empty loads
    -- s_ <- unloadSubs s dl
    -- loadSubs s_ ld

    where
        unsub :: SubscriptionData msg -> IO ()
        unsub (TCPDat { connectedSocket, listenSocket }) = do
            closem connectedSocket
            close listenSocket

        sub :: TSub msg -> IO (SubscriptionData msg)
        sub (TCP p f) = do
            sock <- openTCPPort p
            return $ TCPDat p sock Nothing f

        unloads = substates `Map.restrictKeys` (Set.fromList tsubkeys)

        foldSubs :: SubStates msg -> (Int, TSub msg) -> IO (SubStates msg)
        foldSubs s (k, t) =
            sub t >>= (\s_ -> (return $ Map.insert k s_ s))

        --loads :: [ (Int, TSub msg) ]
        loads =
            filter
                (\(k, _) -> Map.notMember k substates)
                [(k, t) | k <- tsubkeys, t <- tsubs]

        tsubkeys = map hash tsubs

        -- --dl = (\t -> Set.member (hash t) (keys \\ tks))
        -- ld = filter (\t -> Set.member (hash t) (tks \\ keys)) tsubs
        -- ld = s `Map.difference` tsubsmap --TODO: what type should this be?
        -- tks = Set.fromList newkeys
        -- newkeys = map hash tsubs
        -- tsubsmap = Map.fromList $ map (\t -> (hash t, t)) tsubs
        -- keys = Map.keysSet s

--compute difference and union at updateSubscriptions

-- Map Int (TSub msg) -> IO (SubStates msg)
{-
loadSubs :: SubStates msg -> [ TSub msg ] -> IO (SubStates msg)
loadSubs states [] = return states
loadSubs states (x:xs) = do
    state <- loadSub x
    loadSubs (Map.insert (hash x) state states) xs


loadSub :: TSub msg -> IO (SubscriptionData msg)
loadSub (TCP p f) = do
    addr:_ <- getAddrInfo (Just hints) Nothing (Just $ show p)
    sock <- socket AF_INET Stream defaultProtocol
    bind sock (addrAddress addr)
    listen sock 5
    return $ TCPDat p sock Nothing f
-}

openTCPPort :: Port -> IO Socket
openTCPPort p = do
    addr:_ <- getAddrInfo (Just hints) Nothing (Just $ show p)
    sock <- socket AF_INET Stream defaultProtocol
    bind sock (addrAddress addr)
    listen sock 5
    return sock

    where
        hints :: AddrInfo
        hints = defaultHints
            { addrFlags = [AI_PASSIVE]
            , addrFamily = AF_INET
            , addrSocketType = Stream
            }


readSubscriptions :: SubStates msg -> IO (SubStates msg, [ msg ])
readSubscriptions = readSubs . Map.elems
    where
        readSubs :: [ SubscriptionData msg ] -> IO (SubStates msg, [ msg ])
        readSubs [] = return (Map.empty, [])
        readSubs (sd:xsd) = do
            (subdata, mmsg) <- readSub sd
            (states, msgs) <- readSubs xsd
            return ((Map.singleton (hashsd sd) subdata) `Map.union` states, mprepend mmsg msgs)

        hashsd :: SubscriptionData msg -> Int
        hashsd t = hash $ TCP (port t) undefined

        mprepend (Just x) xs = x : xs
        mprepend Nothing xs = xs


readSub :: SubscriptionData msg -> IO (SubscriptionData msg, Maybe msg)
readSub (TCPDat p lsnsc cnsc f) =
    do
        closem cnsc

        (c, _) <- accept $ lsnsc
        bytes <- recvAll c
        return (TCPDat p lsnsc (Just c) f, Just $ f bytes)

    where
        recvAll :: Socket -> IO BS.ByteString
        recvAll = recvAll_ BS.empty

        recvAll_ :: BS.ByteString -> Socket -> IO BS.ByteString
        recvAll_ bss sock = do
            bs <- recv sock 4096
            let result = bss `BS.append` bs

            case BS.length bs of
                0 ->  return $ result
                _ -> recvAll_ result sock


closem :: Maybe Socket -> IO ()
closem = maybe (return ()) close
