{-# LANGUAGE NamedFieldPuns #-}

import Prelude hiding (init)
import Data.Maybe (isJust)
import Data.Array (Array, listArray, indices, (!), (//), assocs)

import Architecture.Cmd (Cmd)
import qualified Architecture.Cmd as Cmd
import Architecture.TEA (Config (..), run)
import Architecture.Sub (Sub)
import qualified Architecture.Sub as Sub
import qualified Mainline.Mainline as M
import Network.KRPC (KPacket (..))
import Network.KRPC.Types (Message (..), NodeInfo (..))

-- Number of nodes on this port
nMplex :: Int
nMplex = 2

type Model = Array Int M.Model

main :: IO ()
main = run config

config :: Config Model M.Msg
config = Config init update subscriptions

init :: (Model, Cmd M.Msg)
init =
    ( listArray (0, nMplex - 1) (replicate nMplex M.Uninitialized)
    , Cmd.batch [(Cmd.randomBytes 20 (M.NewNodeId i)) | i <- [0..nMplex-1]]
    )

subscriptions :: Model -> Sub M.Msg
subscriptions m
    | indices m == [] = Sub.none
    | isUn (m!0) = Sub.none
    | otherwise = Sub.udp M.servePort M.parseReceivedBytes
        where
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
    m
    | null havet = M.logHelper
        (NodeInfo nodeid ci)
        (Response nodeid r)
        "Ignoring a response that wasn't in our transaction state from"
        m
    | otherwise = updateExplicit msg m (fst $ head havet)
        where
            msg = M.Inbound t ci (KPacket transactionId (Response nodeid r) v)

            havet = filter (\(_, model) -> fil model) (assocs m)

            fil (M.Ready state) = fil2 state
            fil (M.Uninitialized1 _ sometid) = sometid == transactionId
            fil _ = False

            fil2 :: M.ServerState -> Bool
            fil2 state = isJust $
                M.getMTstate nodeid (M.transactions state) transactionId


{-
 - This should implicitly handle any message explicitly intended for
 - a particular subcomponent, namely: SendFirstMessage, SendMessage,
 - and SendResponse.
 -}
update msg m = updateExplicit msg m (M.idx msg)

updateExplicit :: M.Msg -> Model -> Int -> (Model, Cmd M.Msg)
updateExplicit msg m ix = (m // [(ix, mm)], cmds)
    where
        (mm, cmds) = M.update msg (m!ix)
