import Prelude hiding (init)
import Data.Time.Clock.POSIX (POSIXTime)

import Architecture.TEA (Config (..), run)
import qualified Architecture.Cmd as Cmd
import Architecture.Sub (Sub)
import qualified Architecture.Sub as Sub

type Model = Float

data Msg
    = Increment Float
    | Got Sub.Received
    | Timeout POSIXTime

init :: (Model, Cmd.Cmd Msg)
init = (0, Cmd.getRandom Increment)

update :: Msg -> Model -> (Model, Cmd.Cmd Msg)
update (Increment i) n = (n + i, Cmd.print n)

update (Got msg) n =
    ( n
    , Cmd.batch
        [ Cmd.getRandom Increment
        , Cmd.log Cmd.INFO [ show (Sub.bytes msg) ]
        ]
    )

update (Timeout now) n = (n , Cmd.log Cmd.INFO [ "Timer! " ++ show now ])

subscriptions :: Model -> Sub Msg
subscriptions n
    | n > 3 = Sub.none
    | otherwise = Sub.batch
        [ Sub.udp 51411 (\_ -> Got)
        , Sub.timer 10000 Timeout
        ]

config :: Config Model Msg
config = Config init update subscriptions

main :: IO ()
main = run config
