{-# LANGUAGE
    DataKinds
  , OverloadedLabels
  , TypeApplications
#-}

import Prelude hiding (init)
import Data.Time.Clock.POSIX (POSIXTime)

import Squeal.PostgreSQL
    ( Query_
    , Public
    , Only (..)
    , param
    , as
    , values_
    , runQueryParams
    , firstRow
    )
import Squeal.PostgreSQL.Pool (PoolPQ)

import Network.KRPC.Helpers (stringpack)
import Architecture.TEA (dbApp)
import qualified Architecture.Cmd as Cmd
import Architecture.Sub (Sub)
import qualified Architecture.Sub as Sub

type Model = Float

data Msg
    = Increment Float
    | Got Sub.Received
    | Timeout POSIXTime
    | PGResult Float

init :: (Model, Cmd.Cmd Msg)
init = (0, Cmd.getRandom Increment)

query :: Query_ (Public '[]) (Only Float) (Only Float)
query = values_ $ ((param @1) + (param @1)) `as` #fromOnly

session :: Float -> PoolPQ (Public '[]) IO Float
session i = do
    result <- runQueryParams query (Only i)
    Just (Only ii) <- firstRow result
    return ii


update :: Msg -> Model -> (Model, Cmd.Cmd Msg)
update (Increment i) n = (n, Cmd.batch [ cmdlog, dbcmd ])
    where
        cmdlog = Cmd.log Cmd.DEBUG [ show n, show i, show $ n + i ]
        dbcmd = Cmd.db (session i) PGResult

update (PGResult ii) n = (n + ii, cmdlog)
    where
        cmdlog = Cmd.log Cmd.INFO [ "DB result:", show ii, show (n + ii) ]

update (Got msg) n =
    ( n
    , Cmd.batch
        [ Cmd.getRandom Increment
        , Cmd.log Cmd.INFO [ show (Sub.bytes msg) ]
        ]
    )

update (Timeout now) n = (n, Cmd.log Cmd.INFO [ "Timer! " ++ show now ])

subscriptions :: Model -> Sub Msg
subscriptions n
    | n > 3 = Sub.none
    | otherwise = Sub.batch
        [ Sub.udp 51411 (\_ -> Got)
        , Sub.timer 10000 Timeout
        ]


main :: IO ()
main =
    dbApp
        init
        update
        subscriptions
        (stringpack
            "host=192.168.4.2 dbname=test user=guest password=invisiblegiraffe")
