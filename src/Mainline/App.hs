{-# LANGUAGE NamedFieldPuns #-}

import Prelude hiding (init, lookup)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Int (Int32)
import qualified Data.Map as Map
import Data.Array (Array, listArray, indices, (!), (//), assocs)
import Data.List (sortBy, foldl')
import Data.Bits (xor)
import Data.Function (on)
import Data.Foldable (toList)
import Data.Cache.LRU (LRU, newLRU, lookup, insert)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Hashable (hashWithSalt, hash)
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8)

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
    )
import Network.Octets (octToByteString)
import Mainline.RoutingTable (RoutingTable (..), nclosest)
import qualified Mainline.ResolveMagnet as R
import qualified Mainline.SQL as SQL

import Data.Torrent
    ( InfoDict (..)
    , FileInfo (..)
    , HashList (..)
    , PieceInfo (..)
    , LayoutInfo (..)
    )

import Debug.Trace (trace)

-- Number of nodes on this port
nMplex :: Int
nMplex = 1

callPerSecondPerCi :: Int
callPerSecondPerCi = 5

scoreAggregateSeconds :: Int
scoreAggregateSeconds = 60

data Model = Model
    { models  :: Array Int M.Model
    , tcache  :: LRU Int POSIXTime
    , haves   :: LRU InfoHash (POSIXTime, Int) -- (last cleared/first detected, counter)
    , queue   :: [(Int, Int, M.Msg)] -- WARN: unbounded
    , metadls :: Map.Map InfoHash (R.Model, Map.Map CompactInfo R.Model) -- TODO: Time these out?
    -- Also if we have too many downloads early but we want to keep the number of nodes high
    -- can add a database queue for metadata dls
    }

data MMsg
    = MMsg M.Msg
    | RMsg R.Msg
    | ProcessQueue POSIXTime
    | DBHasInfo POSIXTime Integer Int Bool

main :: IO ()
main = SQL.runSetup >> dbApp init update subscriptions SQL.connstr

init :: (Model, Cmd MMsg)
init =
    ( Model
        { models = listArray (0, nMplex - 1) (replicate nMplex M.Uninitialized)
        , tcache = newLRU (Just $ fromIntegral $ nMplex * 10)
        , haves = newLRU (Just $ fromIntegral $ nMplex * 10)
        , queue = []
        , metadls = Map.empty
        }
    , Cmd.batch [(Cmd.randomBytes 20 (MMsg . NewNodeId i)) | i <- [0..nMplex-1]]
    )


subscriptions :: Model -> Sub MMsg
subscriptions mm
    | indices mainms == [] = trace "this should never happen" Sub.none
    | isUn (mainms ! 0) = trace "nothing to sub" Sub.none
    | otherwise = Sub.batch $
        (Sub.up RMsg $ Sub.batch rsubs) :
        [ Sub.udp M.servePort (\ci r -> MMsg $ M.parseReceivedBytes ci r)
        , Sub.timer 100 ProcessQueue
        , Sub.timer (60 * 1000) (\t -> MMsg $ M.TimeoutTransactions t)
        , Sub.timer (5 * 60 * 1000) (\t -> MMsg $ M.MaintainPeers t)
        ]

        where
            mainms = models mm

            isUn :: M.Model -> Bool
            isUn (M.Uninitialized) = True
            isUn _ = False

            rsubs :: [ Sub R.Msg ]
            rsubs = map (\(ih, (ci, mdl)) -> R.subscriptions ih ci mdl) rmodels

            rmodels = Map.assocs (metadls mm)
                >>= \(ih, (_, m)) -> zip (repeat ih) (Map.assocs m)


update :: MMsg -> Model -> (Model, Cmd MMsg)
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
                    if now - created > fromIntegral scoreAggregateSeconds
                    then
                        (insert infohash (now, 1) cached , insertScore (n + 1))
                    else (insert infohash (created, n + 1) cached, Cmd.none)
                Nothing ->
                    case Map.lookup infohash (metadls mm) of
                        Just _ -> (cached, Cmd.none)
                        Nothing -> (cached, checkdb)

        cached = haves mm

        checkdb =
            Cmd.db
                (SQL.queryExists (octToByteString infohash))
                (Just $ DBHasInfo now infohash idx)

        insertScore score =
            Cmd.db
                ( SQL.incrementScore
                    (octToByteString infohash)
                    (calculateScore score now)
                )
                Nothing


update (DBHasInfo t infohash _ True) model =
    (model { haves = newHaves }, Cmd.none)
    where
        cached = haves model
        (_, mHaveInfo) = lookup infohash cached
        newHaves = case mHaveInfo of
            Just (created, n) -> insert infohash (created, n + 1) cached
            Nothing -> insert infohash (t, 1) cached


update (DBHasInfo t infohash ix False) model
    | isReady mm =
        ( model
        , Cmd.randomBytes M.tidsize
            (\newid -> MMsg $ SendMessage
                { idx        = ix
                , sendAction = M.GettingPeers infohash
                , targetNode = target
                , body       = Query ourid (GetPeers infohash)
                , newtid     = newid
                , when       = t
                }
            )
        )
    | otherwise = (model, Cmd.none)

        where
            target = head $ nclosest infohash 1 (M.routingTable state)

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

update (MMsg (PeersFoundResult nodeid infohash peers)) model
    | Map.member infohash dls =
        ( model { metadls = Map.insert infohash (newMdls) dls }
        , Cmd.up RMsg (Cmd.batch newcmds)
        )
    | otherwise =
        ( model { metadls = Map.insert infohash (R.Off, initialMdls) dls }
        , Cmd.up RMsg (Cmd.batch cmds)
        )

    where
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

        -- For the case of existing download
        existingInfo = dls Map.! infohash

        existingMdls :: Map.Map CompactInfo R.Model
        existingMdls = snd existingInfo

        newMdls :: (R.Model, Map.Map CompactInfo R.Model)
        newMdls = (fst existingInfo, existingMdls `Map.union` initialMdls)

        newcmds :: [Cmd R.Msg]
        newcmds = map (snd . snd) $
            filter (((flip Map.notMember) existingMdls) . fst) rinfo

update (MMsg UDPError) _ = undefined

update (RMsg (R.Have now infohash infodict)) model =
    ( model
        { metadls = Map.delete infohash (metadls model)
        , haves = insert infohash (now, 1) (haves model)
        }
    , insertdb
    )

    where
        insertdb = Cmd.db insertSchema Nothing

        insertSchema = SQL.insertInfo
            (octToByteString infohash)
            (fromIntegral $ piPieceLength pieceInfo)
            (decodeUtf8 $ getName layoutInfo)
            (calculateScore 1 now)
            (unHashList $ piPieceHashes pieceInfo)
            (getFileTups layoutInfo)

        pieceInfo = idPieceInfo infodict

        layoutInfo = idLayoutInfo infodict

        getName :: LayoutInfo -> ByteString
        getName SingleFile { liFile } = fiName liFile
        getName MultiFile { liDirName } = liDirName

        getFileTups :: LayoutInfo -> [([Text], Int32)]
        getFileTups SingleFile { liFile } =
            [( [decodeUtf8 $ fiName liFile]
            ,  fromIntegral $ fiLength liFile
            )]
        getFileTups MultiFile { liFiles } = map mkFileTup liFiles

        mkFileTup :: FileInfo [ByteString] -> ([Text], Int32)
        mkFileTup FileInfo { fiLength, fiName } =
            ( map decodeUtf8 fiName
            , fromIntegral $ fiLength
            )

update (RMsg (R.TCPError infohash ci)) model =
    ( model { metadls = newdls }
    , Cmd.log Cmd.DEBUG [ "Download of ", show infohash
        ,"from", show ci, "failed" ]
    )
    where
        newdls =
            Map.update
                ( \(a, dls) ->
                    let dls2 = Map.delete ci dls in
                    if Map.null dls2 then Nothing
                    else Just (a, dls2)
                )
                infohash
                (metadls model)

update (RMsg m) model = (model { metadls = newdls }, Cmd.up RMsg cmds)

    where
        (infohash, ci) = details m

        dls = metadls model

        newdls = case Map.null newmodelm of
            True -> Map.delete infohash dls
            False -> Map.insert infohash (newSharedMdl, newmodelm) dls

        (sharedmodel, rmodelm) = dls Map.! infohash

        rmodel2 = rmodelm Map.! ci

        (newmodel, cmds) = case (sharedmodel, rmodel2, m) of
            (R.Downloading {}, R.Downloading {}, _) ->
                R.update m (mergeDls sharedmodel rmodel2)
            (R.Downloading mdataSize _ lastBlk blks, _, (R.Got now _ _ _ msg)) ->
                case R.chooseNextBlk mdataSize lastBlk blks of
                    Nothing ->
                        ( R.Off
                        , Cmd.log Cmd.DEBUG
                            [ "Not starting seemingly finished download" ]
                        )
                    Just nextBlk ->
                        R.update (R.Got now nextBlk infohash ci msg) rmodel2
            _ -> R.update m rmodel2

        newmodelm = case newmodel of
            R.Off -> Map.delete ci rmodelm
            _ -> Map.insert ci newmodel rmodelm

        newSharedMdl = case newmodel of
            R.Downloading {} -> newmodel
            _ -> sharedmodel

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

calculateScore :: Int -> POSIXTime -> Double
calculateScore n t = (euler ** ((log maxDbl / endt) * x) - 1) * (fromIntegral n)
    where
        maxDbl = 10 ** 300
        t0 = 1571335545
        endt = t0 + (1.577 * 10**10)
        x = realToFrac t
        euler = exp 1

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

adaptResult :: (msg0 -> msg1) -> (a, Cmd msg0) -> (a, Cmd msg1)
adaptResult f (model, cmd) = (model, Cmd.up f cmd)

e :: Integer -> Integer -> Integer
e = (^)
