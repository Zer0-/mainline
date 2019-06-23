module Architecture.Internal.Types
    ( SubscriptionData (..)
    , Received (..)
    , InternalState (..)
    , SubState
    ) where

import Data.Map (Map)
import qualified Data.ByteString as BS
import Network.Socket
    ( Socket
    )
import Data.Time.Clock.POSIX (POSIXTime)

import Network.KRPC.Types (Port, CompactInfo)

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


type SubState msg = Map Int (SubscriptionData msg)

data InternalState msg = InternalState
    { subState :: SubState msg
    }
