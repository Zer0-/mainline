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
    loaded <- foldM foldSubs Map.empty loads
    return $
        Map.union (substates `Map.difference` unloads) loaded

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

        loads =
            filter
                (\(k, _) -> Map.notMember k substates)
                [(k, t) | k <- tsubkeys, t <- tsubs]

        tsubkeys = map hash tsubs


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


closem :: Maybe Socket -> IO ()
closem = maybe (return ()) close
