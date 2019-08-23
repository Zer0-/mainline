module Architecture.Internal.Types
    ( Received (..)
    , InternalState (..)
    , SubHandler (..)
    , TCmd (..)
    , Cmd (..)
    , TSub (..)
    , Sub (..)
    , Config (..)
    , CmdQ
    ) where

import Data.Map (Map)
import qualified Data.ByteString as BS
import Network.Socket
    ( Socket
    )
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Hashable (Hashable, hashWithSalt)
import Control.Concurrent (ThreadId)
import Control.Concurrent.STM (TVar, TMVar, TQueue)

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
    = TCPClient CompactInfo (BS.ByteString -> Int) (Received -> msg)
    | UDP Port (CompactInfo -> Received -> msg)
    | Timer Int (POSIXTime -> msg) -- timeout in milliseconds


instance Hashable (TSub msg) where
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


type CmdQ = TQueue (CompactInfo, BS.ByteString)

data SubHandler msg
    = TCPClientHandler
        ( TVar
            ( BS.ByteString -> Int -- getMore
            , Received -> msg --handler
            )
        )
    | UDPHandler (TVar (CompactInfo -> Received -> msg))
    | TimerHandler (TVar (POSIXTime -> msg))


data InternalState msg = InternalState
    { readThreadS  :: Map Int (SubHandler msg, ThreadId)
    , writeThreadS :: TVar (Map Int (CmdQ, ThreadId))
    , sockets      :: Map Int Socket
    , subSink      :: TMVar (Sub msg)
    , cmdSink      :: TQueue (TCmd msg)
    }
