module Architecture.Internal.Sub
    ( TSub (..)
    , Sub (..)
    ) where

import Data.Map (Map)
import Network.Socket (Socket)
import qualified Data.ByteString as BS
import Network.KRPC.Types (Port)


data Subscriptions = Subscriptions
    { boundTCP :: Map Port Socket
    }


data TSub msg
    = TCP Port (BS.ByteString -> msg)


newtype Sub msg = Sub [ TSub msg ]

{-
 - get list of removed subs
 - get list of added subs
 -}
receiveSubs :: Subscriptions -> Sub msg -> IO [ msg ]
receiveSubs subs  (Sub ss) =
    removeSubs subs removeData

    runSubs activeSubs
        where
            activeSubs = undefined --keyset of modified Subscriptions
            removeData
