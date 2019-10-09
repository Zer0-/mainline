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
import Architecture.TEA (dbApp)
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
import Network.Octets (octToByteString)
import Mainline.RoutingTable (RoutingTable (..))
import Mainline.SQL (Schemas);
import qualified Mainline.SQL as SQL

-- Number of nodes on this port
nMplex :: Int
nMplex = 1000

callPerSecondPerCi :: Int
callPerSecondPerCi = 5

scoreAggregateSeconds :: Int
scoreAggregateSeconds = 60

data Model = Model
    { models :: Array Int M.Model
    , tcache :: LRU Int POSIXTime
    , haves  :: LRU Integer (POSIXTime, Int) -- (last cleared/first detected, counter)
    , queue :: [(Int, Int, M.Msg)] -- WARN: unbounded
    }

data MMsg
    = MMsg M.Msg
    | ProcessQueue POSIXTime
    | DBHasInfo Integer Int Bool

main :: IO ()
main = SQL.runSetup >> dbApp init update subscriptions SQL.connstr

init :: (Model, Cmd MMsg Schemas)
init =
    ( Model
        { models = listArray (0, nMplex - 1) (replicate nMplex M.Uninitialized)
        , tcache = newLRU (Just $ fromIntegral $ nMplex * 10)
        , haves = newLRU (Just $ fromIntegral $ nMplex * 10)
        , queue = []
        }
    , Cmd.batch [(Cmd.randomBytes 20 (MMsg . NewNodeId i)) | i <- [0..nMplex-1]]
    )

subscriptions :: Model -> Sub MMsg
subscriptions mm
    | indices m == [] = Sub.none
    | isUn (m!0) = Sub.none
    | otherwise = Sub.batch
        [ Sub.udp M.servePort (\ci r -> MMsg $ M.parseReceivedBytes ci r)
        , Sub.timer 100 ProcessQueue
        , Sub.timer (60 * 1000) (\t -> MMsg $ M.TimeoutTransactions t)
        , Sub.timer (5 * 60 * 1000) (\t -> MMsg $ M.MaintainPeers t)
        ]

        where
            m = models mm
            isUn (M.Uninitialized) = True
            isUn _ = False

update :: MMsg -> Model -> (Model, Cmd MMsg Schemas)
update (MMsg (NewNodeId ix bs)) m = updateExplicit (NewNodeId ix bs) m ix
update (MMsg (ErrorParsing ci bs err)) m =
    (m, Cmd.up MMsg $ M.onParsingErr M.servePort ci bs err)
update
    ( MMsg ( Inbound t ci
        ( KPacket
            { transactionId
            , message = (Response nodeid r)
            , version = v
            }
        )
    ))
    mm
    | null havet = liftResult $ M.logHelper
        (NodeInfo nodeid ci)
        (Response nodeid r)
        "Ignoring a response that wasn't in our transaction state from"
        mm
    | otherwise = updateExplicit msg mm (fst $ head havet)
        where
            m = models mm
            msg = Inbound t ci (KPacket transactionId (Response nodeid r) v)

            havet = filter (fil . snd) (assocs m)

            fil (M.Ready state) = fil2 state
            fil (M.Uninitialized1 _ sometid) = sometid == transactionId
            fil _ = False

            fil2 :: M.ServerState -> Bool
            fil2 state = isJust $
                M.getMTstate nodeid (M.transactions state) transactionId

update
    ( MMsg ( Inbound t ci
        ( KPacket
            { transactionId
            , message =
                ( Query nodeid
                    ( AnnouncePeer
                        impliedPort
                        infohash
                        port
                        token
                        mname
                    )
                )
            , version = v
            }
        )
    ))
    mm = (model { haves = newHaves }, Cmd.batch [ cmds, cmds2 ])

    where
        msg = Inbound t ci (KPacket transactionId (Query nodeid q) v)
        q = AnnouncePeer impliedPort infohash port token mname
        idx = queryToIndex (Query nodeid q) mm
        (model, cmds) = updateExplicit msg mm idx
        (_, mHaveInfo) = lookup infohash cachedInfos

        (newHaves, cmds2) =
            case mHaveInfo of
                Just (created, n) ->
                    if t - created > fromIntegral scoreAggregateSeconds
                    then (newCacheEntry, incrementScore (n + 1))
                    else
                        ( insert infohash (created, n + 1) cachedInfos
                        , Cmd.none
                        )

                Nothing -> (newCacheEntry, checkdb)

        newCacheEntry = insert infohash (t, 1) cachedInfos

        cachedInfos = haves mm

        --incrementScore score = Cmd.db session? Nothing
        incrementScore = undefined

        checkdb =
            Cmd.db
                (SQL.queryExists (octToByteString infohash))
                (Just $ DBHasInfo infohash idx)


update (DBHasInfo _ _ True) model = (model, Cmd.none)
update (DBHasInfo infohash idx False) model = (model, Cmd.none)


update
    ( MMsg ( Inbound t ci
        ( KPacket
            { transactionId
            , message = (Query nodeid q)
            , version = v
            }
        )
    ))
    mm = updateExplicit msg mm (queryToIndex (Query nodeid q) mm)

    where
        msg = Inbound t ci (KPacket transactionId (Query nodeid q) v)

update (MMsg (Inbound _ ci ( KPacket { message }))) m =
    liftResult $ M.logErr ci message m

update (MMsg (SendFirstMessage { idx, sendRecipient, body, newtid })) m =
    sendOrQueue
        (throttle (tcache m) h (fromInteger 0) callPerSecondPerCi)
        h
        msg
        idx
        m

    where
        h = hashci sendRecipient
        msg = SendFirstMessage idx sendRecipient body newtid

update
    (MMsg (SendMessage { idx, sendAction, targetNode, body, newtid, when }))
    m =
        sendOrQueue
            (throttle (tcache m) h when callPerSecondPerCi)
            h
            msg
            idx
            m

        where
            h = hashci $ compactInfo targetNode
            msg = SendMessage idx sendAction targetNode body newtid when

update (MMsg (SendResponse { idx, targetNode, body, tid })) m =
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

update (MMsg (TimeoutTransactions now)) m =
    propagateTimer (TimeoutTransactions now) m

update (MMsg (MaintainPeers now)) m =
    propagateTimer (MaintainPeers now) m


queryToIndex :: Message -> Model -> Int
queryToIndex
    (Query nodeid q)
    mm = fst $ head sorted

    where
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

queryToIndex _ _ = undefined

-- Need! Cmd.up :: Cmd msg schemas0 -> (msg -> mmsg) -> Cmd mmsg

updateExplicit :: Msg -> Model -> Int -> (Model, Cmd MMsg Schemas)
updateExplicit msg model ix =
    ( model { models = m // [(ix, mm)] }
    , Cmd.up MMsg cmds
    )

    where
        m = models model
        (mm, cmds) = M.update msg (m!ix)

propagateTimer :: Msg -> Model -> (Model, Cmd MMsg Schemas)
propagateTimer msg m = (m { models = models2 }, cmds)
    where
        modelCmds = fmap f (models m)
        f = M.update msg

        models2 = fmap fst modelCmds
        cmds = Cmd.batch $ toList $ fmap ((Cmd.up MMsg) . snd) modelCmds

sendOrQueue
    :: Either (LRU Int POSIXTime) (LRU Int POSIXTime)
    -> Int
    -> Msg
    -> Int
    -> Model
    -> (Model, Cmd MMsg Schemas)
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

liftResult :: (a, Cmd Msg schemas) -> (a, Cmd MMsg schemas)
liftResult (model, cmd) = (model, Cmd.up MMsg cmd)

e :: Integer -> Integer -> Integer
e = (^)
