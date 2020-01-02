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
    , PQ
    )

import Squeal.PostgreSQL (Connection)
import Squeal.PostgreSQL.Pool (Pool, createConnectionPool)
import Generics.SOP (K (..))

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
    | PGError

init :: (Model, Cmd.Cmd Msg EmptySchema)
init = (0, Cmd.getRandom Increment)

update :: Msg -> Model -> (Model, Cmd.Cmd Msg EmptySchema)
update (Increment i) n = (n, Cmd.batch [ cmdlog, dbcmd ])
    where
        cmdlog = Cmd.log Cmd.DEBUG [ show n, show i, show $ n + i ]
        dbcmd = Cmd.db (session i) (Right $ maybe PGError PGResult)

update (PGResult ii) n = (n + ii, cmdlog)
    where
        cmdlog = Cmd.log Cmd.INFO [ "DB result:", show ii, show (n + ii) ]

update PGError n = (n, Cmd.log Cmd.WARNING [ "DB Error!" ])

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
        [ Sub.udp 51416 (const Got) (error "udp error")
        , Sub.timer 10000 Timeout
        ]

type EmptySchema = Public '[]

query :: Query_ EmptySchema (Only Float) (Only Float)
query = values_ $ ((param @1) + (param @1)) `as` #fromOnly
--query = values_ $ ((param @1) / 0) `as` #fromOnly


session :: Float -> PQ EmptySchema EmptySchema IO Float
session i = do
    result <- runQueryParams query (Only i)
    Just (Only ii) <- firstRow result
    return ii


createPool :: IO (Pool (K Connection EmptySchema))
createPool = createConnectionPool connstr 1 1 1
    where
      connstr = stringpack
        "host=192.168.4.2 dbname=test user=guest password=invisiblegiraffe"

main :: IO ()
main =
    dbApp
        init
        update
        subscriptions
        createPool
