module Architecture.Internal.Sub
    ( TSub (..)
    , Sub (..)
    , SubStates
    , updateSubscriptions
    , readSubscriptions
    ) where

import Data.Map (Map)
import Data.Set ((\\))
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
updateSubscriptions s (Sub tsubs) =
    loadSubs s ld

    where
        keys = Map.keysSet s
        ld = filter (\t -> Set.member (hash t) (tks \\ keys)) tsubs
        tks = Set.fromList $ map hash tsubs


loadSubs :: SubStates msg -> [ TSub msg ] -> IO (SubStates msg)
loadSubs states [] = return states
loadSubs states (x:xs) = do
    state <- loadSub x
    loadSubs (Map.insert (hash x) state states) xs


hints :: AddrInfo
hints = defaultHints
    { addrFlags = [AI_PASSIVE]
    , addrFamily = AF_INET
    , addrSocketType = Stream
    }

loadSub :: TSub msg -> IO (SubscriptionData msg)
loadSub (TCP p f) = do
    addr:_ <- getAddrInfo (Just hints) Nothing (Just $ show p)
    sock <- socket AF_INET Stream defaultProtocol
    bind sock (addrAddress addr)
    listen sock 5
    return $ TCPDat p sock Nothing f


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
readSub tcpdat =
    do
        case (connectedSocket tcpdat) of
            Just s -> close s
            Nothing -> return ()

        (c, _) <- accept $ listenSocket tcpdat
        bytes <- recvAll c
        return (tcpdat { connectedSocket = Just c }, Just $ (handler tcpdat) $ bytes)

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
