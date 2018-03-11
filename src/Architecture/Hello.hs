import Prelude hiding (init)
import Architecture.TEA (Config (..), run)
import qualified Architecture.Cmd as Cmd

type Model = Int

data Msg = Increment Float

init :: (Model, Cmd.Cmd Msg)
init = (0, Cmd.getRandom Increment)

update :: Msg -> Model -> (Model, Cmd.Cmd Msg)
update (Increment _) n
    | n > 10 = (0, Cmd.exit)
    | otherwise = (n + 1, Cmd.getRandom Increment)

config :: Config Model Msg
config = Config init update

main :: IO ()
main = run config
