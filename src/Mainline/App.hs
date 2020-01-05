{-# LANGUAGE NamedFieldPuns #-}

module Mainline.App
    (main) where

import Prelude hiding (init, lookup)
import Data.Maybe (isJust)
import Data.Int (Int64)
import qualified Data.Map as Map
import Data.Array (Array, listArray, (!), (//), assocs)
import Data.List (sortBy, foldl')
import Data.Bits (xor)
import Data.Function (on)
import Data.Foldable (toList)
import Data.Cache.LRU (LRU, newLRU, lookup, insert)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Hashable (hashWithSalt, hash)
import Data.ByteString (ByteString)

import Data.Torrent
    ( InfoDict (..)
    , FileInfo (..)
    , HashList (..)
    , PieceInfo (..)
    , LayoutInfo (..)
    )
import Squeal.PostgreSQL.Pool (Pool)
import Squeal.PostgreSQL (Connection)
import Generics.SOP (K (..))

import qualified Architecture.Cmd as Cmd
import Architecture.TEA (dbApp)
import Architecture.Sub (Sub)
import qualified Architecture.Sub as Sub
import qualified Mainline.Mainline as M
import Mainline.Mainline (Msg (..), Cmd)
import Network.KRPC (KPacket (..))
import Network.KRPC.Types
    ( Message     (..)
    , NodeInfo    (..)
    , QueryDat    (..)
    , CompactInfo (..)
    , InfoHash
    , Port
    )
import Network.KRPC.Helpers (hexify)
import Network.Octets (octToByteString)
import Mainline.RoutingTable (RoutingTable (..), nclosest)
import qualified Mainline.ResolveMagnet as R
import qualified Mainline.SQL as SQL
import qualified Mainline.Prepopulate as Prepopulate
import qualified Mainline.Config as Conf

import Debug.Trace (trace)

data Model = Model
    { models  :: Array Int M.Model
    , tcache  :: LRU Int POSIXTime
    , haves   :: LRU InfoHash (POSIXTime, Int) -- (last cleared/first detected, counter)
    , queue   :: [(Int, Int, M.Msg)] -- WARN: unbounded
    , metadls :: Map.Map InfoHash (R.Model, Map.Map CompactInfo R.Model, Int) -- TODO: Time these out?
    , servePort :: Port
    , msBetweenCallsToNode :: Int
    , scoreAggregateSeconds :: Int
    }

data MMsg
    = MMsg M.Msg
    | RMsg R.Msg
    | ProcessQueue POSIXTime
    | DBHasInfo POSIXTime Integer Int Bool
    | DBInfoLookupFail
    | DBSetScoreFail
    | DBSaveOK Integer
    | DBSaveFail Integer


main :: Conf.Settings -> IO (Pool (K Connection SQL.Schemas))-> Port -> IO ()
main settings mkPool p = do
    seeds <- Prepopulate.main p
    dbApp (init settings p $ seedNodes seeds) update subscriptions mkPool

    where
        seedNodes seeds = listArray (0, length seeds - 1) seeds

init :: Conf.Settings -> Port -> Array Int CompactInfo -> (Model, Cmd MMsg)
init settings p ss =
    ( Model
        { models = listArray
            (0, n - 1)
            [M.Uninitialized $ mkCfg i | i <- [0..n - 1]]
        , tcache = newLRU (Just $ fromIntegral $ n * 10)
        , haves = newLRU (Just $ fromIntegral $ n * 10)
        , queue = []
        , metadls = Map.empty
        , servePort = p
        , msBetweenCallsToNode = Conf.msBetweenCallsToNode settings
        , scoreAggregateSeconds = Conf.scoreAggregateSeconds settings
        }
    , Cmd.batch [mkCmd i | i <- [0..n - 1]]
    )

    where
        n = Conf.nMplex settings
        mkCmd i = Cmd.randomBytes 20 (MMsg . (NewNodeId i))
        mkCfg ix = M.ServerConfig ix p undefined ss (Conf.bucketSize settings)


subscriptions :: Model -> Sub MMsg
subscriptions mm = Sub.batch $
    [Sub.up RMsg $ Sub.batch rsubs
    , Sub.udp
        (servePort mm)
        (\ci r -> MMsg $ M.parseReceivedBytes ci r)
        (MMsg M.UDPError)
    , Sub.timer 1000 ProcessQueue
    , Sub.timer (60 * 1000) (\t -> MMsg $ M.TimeoutTransactions t)
    , Sub.timer (5 * 60 * 1000) (\t -> MMsg $ M.MaintainPeers t)
    ]

    where
        rsubs :: [ Sub R.Msg ]
        rsubs = map (\(ih, (ci, mdl)) -> R.subscriptions ih ci mdl) rmodels

        rmodels = Map.assocs (metadls mm)
            >>= \(ih, (_, m, _)) -> zip (repeat ih) (Map.assocs m)


update :: MMsg -> Model -> (Model, Cmd MMsg)
update (MMsg (NewNodeId ix bs)) m = updateExplicit (NewNodeId ix bs) m ix
update (MMsg (ErrorParsing ci bs err)) m =
    (m, Cmd.up MMsg $ M.onParsingErr (servePort m) ci bs err)
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
    | null havet = adaptResult MMsg $ M.logHelper
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
    ( MMsg ( Inbound now ci
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
        msg = Inbound now ci (KPacket transactionId (Query nodeid q) v)
        q = AnnouncePeer impliedPort infohash port token mname
        idx = queryToIndex (Query nodeid q) mm
        (model, cmds) = updateExplicit msg mm idx
        (_, mHaveInfo) = lookup infohash cached

        (newHaves, cmds2) =
            case mHaveInfo of
                Just (created, n) ->
                    if now - created > fromIntegral (scoreAggregateSeconds mm)
                    then
                        (insert infohash (now, 1) cached , insertScore (n + 1))
                    else (insert infohash (created, n + 1) cached, Cmd.none)
                Nothing ->
                    case Map.lookup infohash (metadls mm) of
                        Just _ -> (cached, Cmd.none)
                        Nothing -> (cached, Cmd.batch [logmsg, checkdb])

        cached = haves mm

        checkdb =
            Cmd.db
                (SQL.queryExists (octToByteString infohash))
                (Right (maybe DBInfoLookupFail (DBHasInfo now infohash idx)))

        logmsg = Cmd.log Cmd.DEBUG [ "query if db has", show infohash ]

        insertScore score =
            Cmd.db
                ( SQL.incrementScore
                    (octToByteString infohash)
                    (calculateScore score now)
                )
                (Left DBSetScoreFail)


update (DBHasInfo t infohash _ True) model =
    (model { haves = newHaves }, logmsg)
    where
        cached = haves model
        (_, mHaveInfo) = lookup infohash cached
        newHaves = case mHaveInfo of
            Just (created, n) -> insert infohash (created, n + 1) cached
            Nothing -> insert infohash (t, 1) cached

        logmsg = Cmd.log Cmd.DEBUG [ "DB already has", show infohash ]


update (DBHasInfo t infohash ix False) model
    | isReady mm && not (null closest) =
        ( model
        , Cmd.randomBytes M.tidsize
            (\newid -> MMsg $ SendMessage
                { idx        = ix
                , sendAction = M.NewPeersSearch infohash
                , targetNode = head closest
                , body       = Query ourid (GetPeers infohash)
                , newtid     = newid
                , when       = t
                }
            )
        )
    | otherwise = (model, Cmd.none)

        where
            closest = nclosest infohash 1 (M.routingTable state)

            ourid = M.ourId $ M.conf state

            state = getServerState mm

            mm = (models model) ! ix

            isReady :: M.Model -> Bool
            isReady (M.Ready _) = True
            isReady _ = False

            getServerState :: M.Model -> M.ServerState
            getServerState (M.Ready m) = m
            getServerState _ = undefined

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
    adaptResult MMsg $ M.logErr ci message m

update
    (MMsg (SendFirstMessage {idx, sendRecipient, body, newtid}))
    m = sendOrQueue
        (throttle (tcache m) h (fromInteger 0) (msBetweenCallsToNode m))
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
            (throttle (tcache m) h when (msBetweenCallsToNode m))
            h
            msg
            idx
            m

        where
            h = hashci $ compactInfo targetNode
            msg = SendMessage idx sendAction targetNode body newtid when

update (MMsg (SendResponse { idx, targetNode, body, tid })) m =
    updateExplicit (SendResponse idx targetNode body tid) m idx

update (ProcessQueue now) m = foldl' f (m { queue = [] }, Cmd.none) (trace ("t Processing queue of length " ++ (show $ length (queue m))) (queue m))
    where
        f (model, cmds) (i, key, msg) =
            let
                (model2, cmds2) = sendOrQueue
                    (throttle (tcache model) key now (msBetweenCallsToNode m))
                    key
                    msg
                    i
                    model
            in (model2, Cmd.batch [cmds, cmds2])

update (MMsg (TimeoutTransactions now)) m =
    propagateTimer (TimeoutTransactions now) m

update (MMsg (MaintainPeers now)) m =
    propagateTimer (MaintainPeers now) m

update (MMsg (PeersFoundResult idx nodeid infohash peers)) model
    | Map.member infohash dls =
        ( model { metadls = metadlsB }
        , Cmd.batch [Cmd.up RMsg (Cmd.batch newcmds), mklog metadlsB]
        )
    | otherwise =
        ( model { metadls = metadlsA }
        , Cmd.batch [Cmd.up RMsg (Cmd.batch cmds), mklog metadlsA]
        )

    where
        mklog x = Cmd.log Cmd.DEBUG
            [ show $ countOngoingDls x
            , "ongoing metainfo downloads after peersfound"
            ]

        cmds :: [Cmd R.Msg]
        cmds = map snd inits

        initialMdls = Map.fromList (map (\(ci, (mdl, _)) -> (ci, mdl)) rinfo)

        rinfo :: [(CompactInfo, (R.Model, Cmd R.Msg))]
        rinfo = zip peers $ inits

        inits = map rinit peers

        rinit :: CompactInfo -> (R.Model, Cmd R.Msg)
        rinit ci =
            R.update (R.DownloadInfo nodeid infohash ci) R.Off

        dls = metadls model

        metadlsA = Map.insert infohash (R.Off, initialMdls, idx) dls

        -- For the case of existing download
        existingInfo = dls Map.! infohash

        existingMdls :: Map.Map CompactInfo R.Model
        existingMdls = (\(_, x, _) -> x) existingInfo

        newMdls :: (R.Model, Map.Map CompactInfo R.Model, Int)
        newMdls =
            ( (\(x, _, _) -> x) existingInfo
            , existingMdls `Map.union` initialMdls
            , idx
            )

        newcmds :: [Cmd R.Msg]
        newcmds = map (snd . snd) $
            filter (((flip Map.notMember) existingMdls) . fst) rinfo

        metadlsB = Map.insert infohash newMdls dls


update (MMsg UDPError) model = (model, Cmd.log Cmd.WARNING ["UDPError"])

update (MMsg (NodeAdded _)) model = (model, Cmd.none)

update (RMsg (R.Have now infohash infodict)) model =
    ( model
        { metadls = Map.delete infohash (metadls model)
        , haves = insert infohash (now, 1) (haves model)
        , models = newmdls
        }
    , case filetups of
        [] -> errmsg
        _ -> Cmd.batch [logmsg, insertdb]
    )

    where
        insertdb = Cmd.db
            insertSchema
            (Right $ maybe (DBSaveFail infohash) (\_ -> DBSaveOK infohash))

        insertSchema = SQL.insertInfo
            (octToByteString infohash)
            (fromIntegral $ piPieceLength pieceInfo)
            (getName layoutInfo)
            (calculateScore 1 now)
            (unHashList $ piPieceHashes pieceInfo)
            filetups

        filetups = getFileTups layoutInfo

        pieceInfo = idPieceInfo infodict

        layoutInfo = idLayoutInfo infodict

        getName :: LayoutInfo -> ByteString
        getName SingleFile { liFile } = fiName liFile
        getName MultiFile { liDirName } = liDirName

        getFileTups :: LayoutInfo -> [([ByteString], Int64)]
        getFileTups SingleFile { liFile } =
            [( [fiName liFile]
            ,  fromIntegral $ fiLength liFile
            )]
        getFileTups MultiFile { liFiles } = map mkFileTup liFiles

        mkFileTup :: FileInfo [ByteString] -> ([ByteString], Int64)
        mkFileTup FileInfo { fiLength, fiName } =
            (fiName, fromIntegral $ fiLength)

        -- remove gettingPeers entry from particular model
        dlinfo = Map.lookup infohash (metadls model)

        newmdls = case dlinfo of
            Just (_, _, idx) ->
                ms // [(idx, removeGettingPeers (ms ! idx) infohash)]

            Nothing -> ms

        ms = models model

        logmsg = Cmd.log Cmd.INFO
            [ "Calling database to save"
            , show $ hexify $ octToByteString infohash
            ]

        errmsg = Cmd.log Cmd.WARNING
            [ "meta info"
            , show $ hexify $ octToByteString infohash
            , "has an empty file list! Not saving to database."
            ]

update (RMsg (R.TCPError infohash ci)) model =
    ( model { metadls = newdls }
    , Cmd.log Cmd.DEBUG [ "Download of ", show infohash
        ,"from", show ci, "failed" ]
    )
    where
        newdls =
            Map.update
                ( \(a, dls, idx) ->
                    let dls2 = Map.delete ci dls in
                    if Map.null dls2 then Nothing
                    else Just (a, dls2, idx)
                )
                infohash
                (metadls model)

update (RMsg m) model = (model { metadls = newdls }, Cmd.up RMsg cmds)
    where
        (infohash, ci) = details m

        dls = metadls model

        mdl = Map.lookup infohash dls

        (newdls, cmds) = case mdl of
            Just (sharedmodel, rmodelm, idx) ->
                let
                    (newmodel, cmds1) = case Map.lookup ci rmodelm of
                        Just rmodel2 -> updateRModel sharedmodel rmodel2
                        Nothing ->
                            ( R.Off
                            , Cmd.log Cmd.DEBUG
                                [ "CompactInfo for RMsg not in our state." ]
                            )

                    newmodelm = case newmodel of
                        R.Off -> Map.delete ci rmodelm
                        _ -> Map.insert ci newmodel rmodelm

                    newSharedMdl = case newmodel of
                        R.Downloading {} -> newmodel
                        _ -> sharedmodel

                    newdls1 = case Map.null newmodelm of
                        True -> Map.delete infohash dls
                        False -> Map.insert
                            infohash
                            (newSharedMdl, newmodelm, idx)
                            dls

                in
                    (newdls1, cmds1)

            -- something else has deleted our entry
            Nothing -> (dls, Cmd.none)


        updateRModel :: R.Model -> R.Model -> (R.Model, Cmd R.Msg)
        updateRModel sharedmodel rmodel =
            case (sharedmodel, rmodel, m) of
                (R.Downloading {}, R.Downloading {}, _) ->
                    R.update m (mergeDls sharedmodel rmodel)
                (R.Downloading mdataSize _ lastBlk blks, _, (R.Got now _ _ _ msg)) ->
                    case R.chooseNextBlk mdataSize lastBlk blks of
                        Nothing ->
                            ( R.Off
                            , Cmd.log Cmd.DEBUG
                                [ "Not starting seemingly finished download" ]
                            )
                        Just nextBlk ->
                            R.update (R.Got now nextBlk infohash ci msg) rmodel
                _ -> R.update m rmodel


        details :: R.Msg -> (InfoHash, CompactInfo)
        details (R.DownloadInfo _ i c) = (i, c)
        details (R.GotHandshake i c _) = (i, c)
        details (R.Got _ _ i c _) = (i, c)
        details (R.Have _ _ _) = undefined
        details (R.TCPError i c) = (i, c)

        mergeDls :: R.Model -> R.Model -> R.Model
        mergeDls (R.Downloading _ _ lastBlk blks) (R.Downloading sz msgid _ _)
            = R.Downloading sz msgid lastBlk blks
        mergeDls _ _ = undefined

update DBInfoLookupFail model =
    (model, Cmd.log Cmd.WARNING [ "Failed to lookup infohash in database!"])

update DBSetScoreFail model =
    (model, Cmd.log Cmd.WARNING [ "Failed to set new score in database!"])

update (DBSaveOK infohash) model = (model, logmsg)
    where
        logmsg = Cmd.log Cmd.INFO
            [ "Information saved to database OK:"
            , show $ hexify $ octToByteString infohash
            ]

update (DBSaveFail infohash) model = (model, logmsg)
    where
        logmsg = Cmd.log Cmd.WARNING
            [ "FAILED to save info to database:"
            , show $ hexify $ octToByteString infohash
            ]

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
        getid (M.Uninitialized _) = -(2`e`161)

        ms = assocs (models mm)

        fil (M.Ready state) = Map.member nodeid (nodes $ M.routingTable state)
        fil _ = False

queryToIndex _ _ = undefined


updateExplicit :: M.Msg -> Model -> Int -> (Model, Cmd MMsg)
updateExplicit msg model ix =
    ( model { models = m // [(ix, mm)] }
    , Cmd.up MMsg cmds
    )

    where
        m = models model
        (mm, cmds) = M.update msg (m ! ix)


propagateTimer :: M.Msg -> Model -> (Model, Cmd MMsg)
propagateTimer msg m = (m { models = models2 }, cmds)
    where
        modelCmds = fmap f (models m)
        f = M.update msg

        models2 = fmap fst modelCmds
        cmds = Cmd.batch $ toList $ fmap ((Cmd.up MMsg) . snd) modelCmds


sendOrQueue
    :: Either (LRU Int POSIXTime) (LRU Int POSIXTime)
    -> Int
    -> M.Msg
    -> Int
    -> Model
    -> (Model, Cmd MMsg)
sendOrQueue result key msg idx m =
    case result of
        (Left cache) ->
            ( m { tcache = cache, queue = queue m ++ [(idx, key, msg)] }
            , Cmd.none
            )

        (Right cache) -> updateExplicit msg m { tcache = cache } idx


removeGettingPeers :: M.Model -> InfoHash -> M.Model
removeGettingPeers (M.Ready state) infohash =
    M.Ready $ state { M.gettingPeers = newGettingPeers }
    where
        newGettingPeers = Map.delete infohash (M.gettingPeers state)
removeGettingPeers _ _ = undefined


calculateScore :: Int -> POSIXTime -> Double
calculateScore n t = (euler ** ((log maxDbl / endt) * (x - t0)) - 1) * (fromIntegral n)
    where
        maxDbl = 10 ** 300
        t0 = 1572391617
        endt = t0 + (1.577 * 10**10)
        x = realToFrac t
        euler = exp 1

throttle
    :: (Ord a)
    => LRU a POSIXTime
    -> a
    -> POSIXTime
    -> Int -- ms between calls
    -> Either (LRU a POSIXTime) (LRU a POSIXTime)
throttle cache key now ms =
    case lookup key cache of
        (_, Nothing) -> Right cache2
        (lru, Just time) ->
            if now - time < (fromIntegral ms) / (fromIntegral (1000 :: Int))
            then Left lru
            else Right cache
    where
        cache2 = insert key now cache

hashci :: CompactInfo -> Int
hashci (CompactInfo ip port) = hashWithSalt (hash ip) port

adaptResult :: (msg0 -> msg1) -> (a, Cmd msg0) -> (a, Cmd msg1)
adaptResult f (model, cmd) = (model, Cmd.up f cmd)

countOngoingDls :: Map.Map a (b, Map.Map c b, d) -> Int
countOngoingDls m = sum $ map (\(_, s, _) -> Map.size s) (Map.elems m)

e :: Integer -> Integer -> Integer
e = (^)
