import Prelude hiding (init)
import Architecture.TEA (Config (..), run)
import qualified Architecture.Cmd as Cmd
import qualified Architecture.Sub as Sub

type Model = Float

data Msg = Increment Float

init :: (Model, Cmd.Cmd Msg)
init = (0, Cmd.getRandom Increment)

update :: Msg -> Model -> (Model, Cmd.Cmd Msg)
update (Increment i) n
    | n > 10 = (0, Cmd.none)
    | otherwise
        = ( n + i
        , Cmd.batch
            [ Cmd.getRandom Increment
            , Cmd.print n
            ]
        )

config :: Config Model Msg
config = Config init update (\_ -> Sub.none)

main :: IO ()
main = run config
