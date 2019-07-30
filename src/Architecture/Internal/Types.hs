module Architecture.Internal.Types
    ( SubscriptionData (..)
    , Received (..)
    , InternalState (..)
    , SubState
    , TCmd (..)
    , Cmd (..)
    , TSub (..)
    , Sub (..)
    , Config (..)
    ) where

import Data.Map (Map)
import qualified Data.ByteString as BS
import Network.Socket
    ( Socket
    )
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Hashable (Hashable, hashWithSalt)
import Control.Concurrent.STM (TVar)

import Network.KRPC.Types (Port, CompactInfo)

data TCmd msg
    = CmdLog String
    | CmdGetRandom (Float -> msg)
    | CmdGetTime (POSIXTime -> msg)
    | CmdRandomBytes Int (BS.ByteString -> msg)
    | CmdSendUDP Port CompactInfo BS.ByteString
    | CmdSendTCP CompactInfo BS.ByteString
    | CmdReadFile String (BS.ByteString -> msg)
    | CmdWriteFile String BS.ByteString

newtype Cmd msg = Cmd [ TCmd msg ]

data TSub msg
    = TCP Port (BS.ByteString -> msg)
    | TCPClient CompactInfo (BS.ByteString -> Int) (Received -> msg)
    | UDP Port (CompactInfo -> Received -> msg)
    | Timer Int (POSIXTime -> msg) -- timeout in milliseconds

instance Hashable (TSub msg) where
    hashWithSalt s (TCP p _) = s `hashWithSalt` (0 :: Int) `hashWithSalt` p
    hashWithSalt s (TCPClient ci _ _) = s `hashWithSalt` (1 :: Int) `hashWithSalt` ci
    hashWithSalt s (UDP p _) = s `hashWithSalt` (2 :: Int) `hashWithSalt` p
    hashWithSalt s (Timer t _) = s `hashWithSalt` (3 :: Int) `hashWithSalt` t

newtype Sub msg = Sub [ TSub msg ]

data Config model msg =
    Config
    { init          :: (TVar model, Cmd msg)
    , update        :: msg -> model -> (model, Cmd msg)
    , subscriptions :: model -> Sub msg
    }

data Received = Received
    { bytes :: BS.ByteString
    , time  :: POSIXTime
    }


data SubscriptionData msg
    = TCPDat
        { port :: Port
        , listenSocket :: Socket
        , connectedSocket :: Maybe Socket
        , tcpHandler :: (BS.ByteString -> msg)
        }
    | TCPClientDat
        { info :: CompactInfo
        , clientSocket :: Socket -- connectedSocket but that name is taken
        , getMore :: (BS.ByteString -> Int)
        , clientHandler :: (Received -> msg)
        }
    | UDPDat
        { port :: Port
        , boundSocket :: Socket
        , udpHandler :: (CompactInfo -> Received -> msg)
        }
    | TimerDat
        { ms :: Int
        , timerHandler :: (POSIXTime -> msg)
        , lastTime :: POSIXTime
        }


-- Perhaps this should be a map of Int (hash) -> Handler
-- where data Handler
--      = TCPDatHandler (BS.ByteString -> msg)
--      | TimerDatHandler (POSIXTime -> msg)
--      ... etc
-- And the rest of the data is thread local
type SubState msg = Map Int (SubscriptionData msg)

newtype InternalState msg = InternalState
    { subState :: SubState msg
    }
