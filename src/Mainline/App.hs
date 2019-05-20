import Prelude hiding (init)
import Data.Array (Array, listArray, (!), (//))

import Architecture.Cmd (Cmd)
import qualified Architecture.Cmd as Cmd
import Architecture.TEA (Config (..), run)
import Architecture.Sub (Sub)
import qualified Architecture.Sub as Sub
import qualified Mainline.Mainline as M

-- Number of nodes on this port
nMplex :: Int
nMplex = 2

type Model = Array Int M.Model

main :: IO ()
main = run config

config :: Config Model M.Msg
config = Config init update subscriptions

init :: (Model, Cmd.Cmd M.Msg)
init =
    ( listArray (0, nMplex) (replicate nMplex M.Uninitialized)
    , Cmd.batch [(Cmd.randomBytes 20 (M.NewNodeId i)) | i <- [0..nMplex]]
    )

subscriptions :: Model -> Sub M.Msg
subscriptions m
    | isUn (m!0) = Sub.none
    | otherwise = Sub.udp M.servePort M.parseReceivedBytes
        where
            isUn (M.Uninitialized) = True
            isUn _ = False

update :: M.Msg -> Model -> (Model, Cmd M.Msg)
update (M.NewNodeId ix bs) m = updateExplicit (M.NewNodeId ix bs) m ix
update (M.ErrorParsing ci bs err) m = (m, M.logParsingErr ci bs err)

updateExplicit :: M.Msg -> Model -> Int -> (Model, Cmd M.Msg)
updateExplicit msg m ix = (m // [(ix, mm)], cmds)
    where
        (mm, cmds) = M.update msg (m!ix)
