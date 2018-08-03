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
    , SocketType (Stream, Datagram)
    , SockAddr
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
import Network.Socket.ByteString (recv, recvFrom)
import Data.Hashable
import qualified Data.ByteString as BS

import Network.KRPC.Types (Port)

--MAXLINE = 65507 -- Max size of a UDP datagram
--(limited by 16 bit length part of the header field)
maxline :: Int
maxline = 2048

data SubscriptionData msg
    = TCPDat
        { port :: Port
        , listenSocket :: Socket
        , connectedSocket :: Maybe Socket
        , handler :: (BS.ByteString -> msg)
        }
    | UDPDat
        { port :: Port
        , boundSocket :: Socket
        , udpHandler :: (SockAddr -> BS.ByteString -> msg)
        }


data TSub msg
    = TCP Port (BS.ByteString -> msg)
    | UDP Port (SockAddr -> BS.ByteString -> msg)

instance Hashable (TSub msg) where
    hashWithSalt s (TCP p _) = s `hashWithSalt` (0::Int) `hashWithSalt` p
    hashWithSalt s (UDP p _) = s `hashWithSalt` (1::Int) `hashWithSalt` p


type SubStates msg = Map Int (SubscriptionData msg)


newtype Sub msg = Sub [ TSub msg ]


updateSubscriptions :: SubStates msg -> Sub msg -> IO (SubStates msg)
updateSubscriptions substates (Sub tsubs) = do
    mapM_ unsub unloads
    loaded <- foldM foldSubs Map.empty loads
    return $
        Map.union (substates `Map.difference` unloads) loaded

    where
        unsub :: SubscriptionData msg -> IO ()
        unsub (TCPDat { connectedSocket, listenSocket }) = do
            closem connectedSocket
            close listenSocket

        unsub (UDPDat { boundSocket }) = do
            close boundSocket

        sub :: TSub msg -> IO (SubscriptionData msg)
        sub (TCP p f) = do
            sock <- openTCPPort p
            return $ TCPDat p sock Nothing f

        sub (UDP p f) = do
            sock <- openUDPPort p
            return $ UDPDat p sock f

        unloads = substates `Map.withoutKeys` (Set.fromList tsubkeys)

        foldSubs :: SubStates msg -> (Int, TSub msg) -> IO (SubStates msg)
        foldSubs s (k, t) =
            sub t >>= (\s_ -> (return $ Map.insert k s_ s))

        loads =
            filter
                (\(k, _) -> Map.notMember k substates)
                [(k, t) | k <- tsubkeys, t <- tsubs]

        tsubkeys = map hash tsubs


bindSocket :: SocketType -> Port -> IO Socket
bindSocket t p = do
    addr:_ <- getAddrInfo (Just hints) Nothing (Just $ show p)
    sock <- socket AF_INET t defaultProtocol
    bind sock (addrAddress addr)
    return sock

    where
        hints :: AddrInfo
        hints = defaultHints
            { addrFlags = [AI_PASSIVE]
            , addrFamily = AF_INET
            , addrSocketType = t
            }


openTCPPort :: Port -> IO Socket
openTCPPort p = do
    sock <- bindSocket Stream p
    listen sock 5
    return sock


openUDPPort :: Port -> IO Socket
openUDPPort = bindSocket Datagram


readSubscriptions :: SubStates msg -> IO (SubStates msg, [ msg ])
readSubscriptions = (foldM ff (Map.empty, [])) . Map.assocs
    where
        ff ::
            (SubStates msg, [ msg ]) ->
            (Int, SubscriptionData msg) ->
            IO (SubStates msg, [ msg ])
        ff (states, msgs) (key, value) =
            readSub value >>=
                ( \(d, mmsg)
                -> return (Map.insert key d states, maybe msgs (: msgs) mmsg)
                )


readSub :: SubscriptionData msg -> IO (SubscriptionData msg, Maybe msg)
readSub (TCPDat p lsnsc cnsc f) =
    do
        closem cnsc

        (c, _) <- accept $ lsnsc
        bytes <- recvAll BS.empty c
        return (TCPDat p lsnsc (Just c) f, Just $ f bytes)

    where
        recvAll :: BS.ByteString -> Socket -> IO BS.ByteString
        recvAll bss sock = do
            bs <- recv sock 4096

            case BS.length bs of
                0 ->  return bss
                _ -> recvAll (bss `BS.append` bs) sock

readSub (UDPDat { port, boundSocket, udpHandler }) =
    recvFrom boundSocket maxline >>=
        (\(bs, sockaddr) ->
            return (UDPDat port boundSocket udpHandler, Just $ udpHandler sockaddr bs))


closem :: Maybe Socket -> IO ()
closem = maybe (return ()) close
