import Prelude hiding (init)
import Data.ByteString (ByteString)
import Architecture.TEA (Config (..), run)
import qualified Architecture.Cmd as Cmd
import Architecture.Sub (Sub)
import qualified Architecture.Sub as Sub

type Model = Float

data Msg
    = Increment Float
    | Received ByteString

init :: (Model, Cmd.Cmd Msg)
init = (0, Cmd.getRandom Increment)

update :: Msg -> Model -> (Model, Cmd.Cmd Msg)
update (Increment i) n = (n + i, Cmd.print n)

update (Received msg) n =
    ( n
    , Cmd.batch
        [ Cmd.getRandom Increment
        , Cmd.log Cmd.INFO [ show msg ]
        ]
    )

subscriptions :: Model -> Sub Msg
subscriptions n
    | n > 3 = Sub.none
    | otherwise = Sub.tcp 8888 Received

config :: Config Model Msg
config = Config init update subscriptions

main :: IO ()
main = run config
