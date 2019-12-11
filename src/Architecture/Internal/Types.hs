{-# LANGUAGE
    DataKinds
  , KindSignatures
  , ExistentialQuantification
#-}

module Architecture.Internal.Types
    ( Received (..)
    , InternalState (..)
    , SubHandler (..)
    , TCmd (..)
    , Cmd (..)
    , TSub (..)
    , Sub (..)
    , Program (..)
    , CmdQ (..)
    , SocketMood (..)
    ) where

import Data.Map (Map)
import qualified Data.ByteString as BS
import Network.Socket (Socket)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Hashable (Hashable, hashWithSalt)
import Control.Concurrent (ThreadId)
import Control.Concurrent.STM (TVar, TMVar, TQueue)
import Squeal.PostgreSQL (Connection)
import Squeal.PostgreSQL (Pool, PQ)
import Generics.SOP (K (..))

import Network.KRPC.Types (Port, CompactInfo)


data TCmd msg schemas
    = CmdLog String
    | CmdGetRandom (Float -> msg)
    | CmdGetTime (POSIXTime -> msg)
    | CmdRandomBytes Int (BS.ByteString -> msg)
    | CmdSendUDP Port CompactInfo BS.ByteString msg
    | forall t. Hashable t => CmdSendTCP t CompactInfo BS.ByteString msg
    | CmdReadFile String (BS.ByteString -> msg)
    | CmdWriteFile String BS.ByteString
    | forall result.
        CmdDatabase
            (PQ schemas schemas IO result)
            (Either msg (Maybe result -> msg))
    | CmdBounce msg
    | SocketResult Int (Maybe Socket)
    | QuitW Int


newtype Cmd msg schemas = Cmd [ TCmd msg schemas ]


data TSub msg
    = forall t u. (Hashable t, Hashable u) =>
        TCPClient t CompactInfo u (BS.ByteString -> Int) (Received -> msg) msg
    | UDP Port (CompactInfo -> Received -> msg)
    | Timer Int (POSIXTime -> msg) -- timeout in milliseconds


instance Hashable (TSub msg) where
    hashWithSalt s (TCPClient t ci _ _ _ _) =
        s `hashWithSalt` (1 :: Int) `hashWithSalt` t `hashWithSalt` ci
    hashWithSalt s (UDP p _) = s `hashWithSalt` (2 :: Int) `hashWithSalt` p
    hashWithSalt s (Timer t _) = s `hashWithSalt` (3 :: Int) `hashWithSalt` t


newtype Sub msg = Sub [ TSub msg ]

data Program model msg schemas =
    Program
    { init          :: (TVar model, Cmd msg schemas)
    , update        :: msg -> model -> (model, Cmd msg schemas)
    , subscriptions :: model -> Sub msg
    }


data Received = Received
    { bytes :: BS.ByteString
    , time  :: POSIXTime
    }


data CmdQ = UDPQueue (TQueue (CompactInfo, BS.ByteString))
          | TCPQueue (TQueue BS.ByteString)

data SubHandler msg
    = TCPClientHandler
        ( TVar
            ( BS.ByteString -> Int -- getMore
            , Received -> msg --handler
            )
        )
    | UDPHandler (TVar (CompactInfo -> Received -> msg))
    | TimerHandler (TVar (POSIXTime -> msg))


data SocketMood msg schemas
    = WantWrites [TCmd msg schemas]
    | WantReads ThreadId (TSub msg)
    | WantBoth ([TCmd msg schemas], TSub msg)
    | HaveSocket Socket


data InternalState msg schemas = InternalState
    { readThreadS  :: Map Int (SubHandler msg, ThreadId)
    , writeThreadS :: TVar (Map Int (CmdQ, TVar Bool, ThreadId))
    , sockets      :: Map Int (SocketMood msg schemas)
    , dbPool       :: Maybe (Pool (K Connection schemas))
    , subSink      :: TMVar (Sub msg)
    , cmdSink      :: TQueue (TCmd msg schemas)
    , curSubHash   :: Int
    }
