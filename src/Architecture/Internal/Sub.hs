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
import Network.Socket (Socket)
import Data.Hashable
import qualified Data.ByteString as BS

import Network.KRPC.Types (Port)


data SubscriptionData msg
    = TCPDat Port Socket (BS.ByteString -> msg)


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

loadSub :: TSub msg -> IO (SubscriptionData msg)
loadSub (TCP port f) = undefined


readSubscriptions :: SubStates msg -> IO [ msg ]
readSubscriptions = undefined
