{-# LANGUAGE NamedFieldPuns #-}

import Prelude hiding (init, lookup)
import Data.Maybe (isJust)
import qualified Data.Map as Map
import Data.Array (Array, listArray, indices, (!), (//), assocs)
import Data.List (sortBy, foldl')
import Data.Bits (xor)
import Data.Function (on)
import Data.Foldable (toList)
import Data.Cache.LRU (LRU, newLRU, lookup, insert)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Hashable (hashWithSalt, hash)

import Architecture.Cmd (Cmd)
import qualified Architecture.Cmd as Cmd
import Architecture.TEA (Config (..), run)
import Architecture.Sub (Sub)
import qualified Architecture.Sub as Sub
import qualified Mainline.Mainline as M
import Mainline.Mainline (Msg (..))
import Network.KRPC (KPacket (..))
import Network.KRPC.Types
    ( Message     (..)
    , NodeInfo    (..)
    , QueryDat    (..)
    , CompactInfo (..)
    )
import Mainline.RoutingTable (RoutingTable (..))

-- Number of nodes on this port
nMplex :: Int
nMplex = 2000

callPerSecondPerCi :: Int
callPerSecondPerCi = 2

data Model = Model
    { models :: Array Int M.Model
    , tcache :: LRU Int POSIXTime
    , queue :: [(Int, Int, M.Msg)] -- WARN: unbounded
    }

main :: IO ()
main = run config

config :: Config Model M.Msg
config = Config init update subscriptions

init :: (Model, Cmd M.Msg)
init =
    ( Model
        { models = listArray (0, nMplex - 1) (replicate nMplex M.Uninitialized)
        , tcache = newLRU (Just $ fromIntegral $ nMplex * 10)
        , queue = []
        }
    , Cmd.batch [(Cmd.randomBytes 20 (M.NewNodeId i)) | i <- [0..nMplex-1]]
    )

subscriptions :: Model -> Sub M.Msg
subscriptions mm
    | indices m == [] = Sub.none
    | isUn (m!0) = Sub.none
    | otherwise = Sub.batch
        [ Sub.udp M.servePort M.parseReceivedBytes
        , Sub.timer 500 M.ProcessQueue
        , Sub.timer (60 * 1000) M.TimeoutTransactions
        , Sub.timer (5 * 60 * 1000) M.MaintainPeers
        ]

        where
            m = models mm
            isUn (M.Uninitialized) = True
            isUn _ = False

update :: M.Msg -> Model -> (Model, Cmd M.Msg)
update (M.NewNodeId ix bs) m = updateExplicit (M.NewNodeId ix bs) m ix
update (M.ErrorParsing ci bs err) m = (m, M.onParsingErr M.servePort ci bs err)
update
    ( M.Inbound t ci
        ( KPacket
            { transactionId
            , message = (Response nodeid r)
            , version = v
            }
        )
    )
    mm
    | null havet = M.logHelper
        (NodeInfo nodeid ci)
        (Response nodeid r)
        "Ignoring a response that wasn't in our transaction state from"
        mm
    | otherwise = updateExplicit msg mm (fst $ head havet)
        where
            m = models mm
            msg = M.Inbound t ci (KPacket transactionId (Response nodeid r) v)

            havet = filter (fil . snd) (assocs m)

            fil (M.Ready state) = fil2 state
            fil (M.Uninitialized1 _ sometid) = sometid == transactionId
            fil _ = False

            fil2 :: M.ServerState -> Bool
            fil2 state = isJust $
                M.getMTstate nodeid (M.transactions state) transactionId

update
    ( M.Inbound t ci
        ( KPacket
            { transactionId
            , message = (Query nodeid q)
            , version = v
            }
        )
    )
    mm = updateExplicit msg mm (fst $ head sorted)

    where
        msg = M.Inbound t ci (KPacket transactionId (Query nodeid q) v)

        know = filter (fil . snd) ms

        sorted = if null know then closest ms else closest know

        closest :: [(Int, M.Model)] -> [(Int, M.Model)]
        closest = sortBy (sortg `on` (getid . snd))

        sortg i j
            | i == j      = EQ
            | cf i < cf j = LT
            | otherwise   = GT

        cf = case q of
            FindNode       nid       -> af nid
            GetPeers       ifo       -> af ifo
            AnnouncePeer _ ifo _ _ _ -> af ifo
            _                        -> bf

        af aux i = min (nodeid `xor` i) (aux `xor` i)
        bf = xor nodeid

        getid (M.Ready state) = M.ourId $ M.conf state
        getid (M.Uninitialized1 conf _) = M.ourId conf
        getid (M.Uninitialized) = -(2`e`161)

        ms = assocs (models mm)

        fil (M.Ready state) = Map.member nodeid (nodes $ M.routingTable state)
        fil _ = False

update ( M.Inbound _ ci ( KPacket { message })) m = M.logErr ci message m

update (SendFirstMessage {idx, sendRecipient, body, newtid}) m =
    sendOrQueue
        (throttle (tcache m) h (fromInteger 0) callPerSecondPerCi)
        h
        msg
        idx
        m

    where
        h = hashci sendRecipient
        msg = SendFirstMessage idx sendRecipient body newtid

update (SendMessage {idx, sendAction, targetNode, body, newtid, when}) m =
    sendOrQueue
        (throttle (tcache m) h when callPerSecondPerCi)
        h
        msg
        idx
        m

    where
        h = hashci $ compactInfo targetNode
        msg = SendMessage idx sendAction targetNode body newtid when

update (SendResponse {idx, targetNode, body, tid}) m =
    updateExplicit (SendResponse idx targetNode body tid) m idx

update (ProcessQueue now) m = foldl' f (m { queue = [] }, Cmd.none) (queue m)
    where
        f (model, cmds) (i, key, msg) =
            let
                (model2, cmds2) = sendOrQueue
                    (throttle (tcache model) key now callPerSecondPerCi)
                    key
                    msg
                    i
                    model
            in (model2, Cmd.batch [cmds, cmds2])

update (M.TimeoutTransactions now) m =
    propagateTimer (M.TimeoutTransactions now) m

update (M.MaintainPeers now) m =
    propagateTimer (M.MaintainPeers now) m


updateExplicit :: M.Msg -> Model -> Int -> (Model, Cmd M.Msg)
updateExplicit msg model ix = (model { models = m // [(ix, mm)] }, cmds)
    where
        m = models model
        (mm, cmds) = M.update msg (m!ix)

propagateTimer :: M.Msg -> Model -> (Model, Cmd M.Msg)
propagateTimer msg m = (m { models = models2 }, cmds)
    where
        modelCmds = fmap f (models m)
        f = M.update msg

        models2 = fmap fst modelCmds
        cmds = Cmd.batch $ toList $ fmap snd modelCmds

sendOrQueue
    :: Either (LRU Int POSIXTime) (LRU Int POSIXTime)
    -> Int
    -> M.Msg
    -> Int
    -> Model
    -> (Model, Cmd M.Msg)
sendOrQueue result key msg idx m =
    case result of
        (Left cache) ->
            ( m { tcache = cache, queue = queue m ++ [(idx, key, msg)] }
            , Cmd.none
            )

        (Right cache) -> updateExplicit msg m { tcache = cache } idx


throttle
    :: (Ord a)
    => LRU a POSIXTime
    -> a
    -> POSIXTime
    -> Int -- Maximum calls per second
    -> Either (LRU a POSIXTime) (LRU a POSIXTime)
throttle cache key now cps =
    case lookup key cache of
        (_, Nothing) -> Right cache2
        (_, Just time) ->
            if now - time < (fromInteger 1) / (fromIntegral cps)
            then Left cache2
            else Right cache2
    where
        cache2 = insert key now cache

hashci :: CompactInfo -> Int
hashci (CompactInfo ip port) = hashWithSalt (hash ip) port

e :: Integer -> Integer -> Integer
e = (^)
