{-# LANGUAGE NamedFieldPuns #-}

import Prelude hiding (init)
import Data.Maybe (isJust)
import qualified Data.Map as Map
import Data.Array (Array, listArray, indices, (!), (//), assocs)
import Data.List (sortBy)
import Data.Bits (xor)
import Data.Function (on)
import Data.Cache.LRU (LRU, newLRU)
import Data.Time.Clock.POSIX (POSIXTime)

import Architecture.Cmd (Cmd)
import qualified Architecture.Cmd as Cmd
import Architecture.TEA (Config (..), run)
import Architecture.Sub (Sub)
import qualified Architecture.Sub as Sub
import qualified Mainline.Mainline as M
import Mainline.Mainline (Msg (..))
import Network.KRPC (KPacket (..))
import Network.KRPC.Types (Message (..), NodeInfo (..), QueryDat (..))
import Mainline.RoutingTable (RoutingTable (..))

-- Number of nodes on this port
nMplex :: Int
nMplex = 100

data Model = Model
    { models :: Array Int M.Model
    , tcache :: LRU Int POSIXTime
    , queue :: [M.Msg]
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
    | otherwise = Sub.udp M.servePort M.parseReceivedBytes
        where
            m = models mm
            isUn (M.Uninitialized) = True
            isUn _ = False

update :: M.Msg -> Model -> (Model, Cmd M.Msg)
update (M.NewNodeId ix bs) m = updateExplicit (M.NewNodeId ix bs) m ix
update (M.ErrorParsing ci bs err) m = (m, M.logParsingErr ci bs err)
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
            FindNode       nid     -> af nid
            GetPeers       ifo     -> af ifo
            AnnouncePeer _ ifo _ _ -> af ifo
            _                      -> bf

        af aux i = min (nodeid `xor` i) (aux `xor` i)
        bf = xor nodeid

        getid (M.Ready state) = M.ourId $ M.conf state
        getid (M.Uninitialized1 conf _) = M.ourId conf
        getid (M.Uninitialized) = -(((^) :: Integer -> Integer -> Integer) 2 161)

        ms = assocs (models mm)

        fil (M.Ready state) = Map.member nodeid (nodes $ M.routingTable state)
        fil _ = False

update ( M.Inbound _ ci ( KPacket { message })) m = M.logErr ci message m

update (SendFirstMessage {idx, sendRecipient, body, newtid}) m =
    updateExplicit (SendFirstMessage idx sendRecipient body newtid) m idx

update (SendMessage {idx, sendAction, targetNode, body, newtid, when}) m =
    updateExplicit (SendMessage idx sendAction targetNode body newtid when) m idx

update (SendResponse {idx, targetNode, body, tid}) m =
    updateExplicit (SendResponse idx targetNode body tid) m idx


updateExplicit :: M.Msg -> Model -> Int -> (Model, Cmd M.Msg)
updateExplicit msg model ix = (model { models = m // [(ix, mm)] }, cmds)
    where
        m = models model
        (mm, cmds) = M.update msg (m!ix)
