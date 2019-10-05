{-# LANGUAGE
    DataKinds
  , KindSignatures
  , ExistentialQuantification
  , RankNTypes
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
    ) where

import Data.Map (Map)
import qualified Data.ByteString as BS
import Network.Socket (Socket)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Hashable (Hashable, hashWithSalt)
import Control.Concurrent (ThreadId)
import Control.Concurrent.STM (TVar, TMVar, TQueue)
import Squeal.PostgreSQL (SchemasType, Connection)
import Squeal.PostgreSQL.Pool (Pool, PoolPQ)
import Generics.SOP (K (..))

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
    | forall schemas result.
        CmdDatabase (PoolPQ (schemas :: SchemasType) IO result) (result -> msg)
    | QuitW Int


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


data Program model msg =
    Program
    { init          :: (TVar model, Cmd msg)
    , update        :: msg -> model -> (model, Cmd msg)
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


data InternalState msg schemas = InternalState
    { readThreadS  :: Map Int (SubHandler msg, ThreadId)
    , writeThreadS :: TVar (Map Int (CmdQ, TVar Bool, ThreadId))
    , sockets      :: Map Int Socket
    , dbPool       :: forall schemas. Maybe (Pool (K Connection (schemas :: SchemasType)))
    , subSink      :: TMVar (Sub msg)
    , cmdSink      :: TQueue (TCmd msg)
    }
