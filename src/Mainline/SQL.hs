{-# LANGUAGE
    DataKinds
  , TypeOperators
  , OverloadedLabels
  , OverloadedStrings
  , TypeApplications
#-}

module Mainline.SQL
    ( Schemas
    , runSetup
    , connstr
    , queryExists
    , qInfoExists
    ) where

import Generics.SOP (NP (..))
import Data.Int (Int32)
import Data.Maybe (isJust)
import Data.ByteString (ByteString)
import Squeal.PostgreSQL.Pool (PoolPQ)
import Squeal.PostgreSQL
    ( Public
    , SchemumType (..)
    , (:::)
    , (:=>)
    , TableConstraint (..)
    , ColumnConstraint (..)
    , NullityType (..)
    , PGType (..)
    , Definition
    , createTableIfNotExists
    , as
    , serial
    , bytea
    , timestampWithTimeZone
    , notNullable
    , default_
    , currentTimestamp
    , doublePrecision
    , check
    , unique
    , primaryKey
    , length_
    , (.==)
    , (&)
    , (>>>)
    , integer
    , vararray
    , text
    , foreignKey
    , OnDeleteClause (..)
    , OnUpdateClause (..)
    , rem_
    , withConnection
    , define
    , Query_
    , select_
    , param
    , from
    , table
    , where_
    , runQueryParams
    , Only (..)
    , firstRow
    , update_
    , Optional (..)
    , Manipulation_
    , manipulateParams_
    , Result
    )

connstr :: ByteString
connstr = "host=192.168.4.2 dbname=test user=guest password=invisiblegiraffe"

type Schemas = Public Schema

type Schema =
    '[ "meta_info"   ::: 'Table (MetaInfoConstraints   :=> MetaInfoColumns)
    ,  "file_info"   ::: 'Table (FileInfoConstraints   :=> FileInfoColumns)
    ,  "info_pieces" ::: 'Table (InfoPiecesConstraints :=> InfoPiecesColumns)
    ]

type MetaInfoConstraints =
    '[ "hash_len"         ::: 'Check      '["info_hash"]
    ,  "info_pk"          ::: 'PrimaryKey '["info_id"]
    ,  "info_hash_unique" ::: 'Unique     '["info_hash"]
    ]

type MetaInfoColumns =
    '[ "info_id"   ::: 'Def   :=> 'NotNull 'PGint4
    ,  "info_hash" ::: 'NoDef :=> 'NotNull 'PGbytea
    ,  "added"     ::: 'Def   :=> 'NotNull 'PGtimestamptz
    ,  "score"     ::: 'Def   :=> 'NotNull 'PGfloat8
    ]

type FileInfoConstraints =
    '[ "file_pk" ::: 'PrimaryKey '["info_id", "filepath"]
    ,  "file_fk" ::: 'ForeignKey '["info_id"] "meta_info" '["info_id"]
    ]

type FileInfoColumns =
    '[ "info_id"    ::: 'NoDef :=> 'NotNull 'PGint4
    ,  "filepath"   ::: 'NoDef :=> 'NotNull ('PGvararray ('NotNull 'PGtext))
    ,  "size_bytes" ::: 'NoDef :=> 'NotNull 'PGint4
    ]

type InfoPiecesConstraints =
    '[ "pieces_divisible" ::: 'Check '["pieces"]
    , "pieces_pk" ::: 'PrimaryKey '["info_id"]
    , "pieces_fk" ::: 'ForeignKey '["info_id"] "meta_info" '["info_id"]
    ]

type InfoPiecesColumns =
    '[ "info_id" ::: 'NoDef :=> 'NotNull 'PGint4
    ,  "pieces"  ::: 'NoDef :=> 'NotNull 'PGbytea
    ]

setup :: Definition Schemas Schemas
setup =
    createTableIfNotExists #meta_info
        (  serial `as` #info_id
        :* ( bytea & notNullable ) `as` #info_hash
        :* ( timestampWithTimeZone
                & notNullable & default_ currentTimestamp ) `as` #added
        :* ( doublePrecision & notNullable & default_ 0 ) `as` #score
        )
        ( check #info_hash (length_ #info_hash .== 20) `as` #hash_len
        :* primaryKey #info_id `as` #info_pk
        :* unique #info_hash `as` #info_hash_unique
        ) >>>

    createTableIfNotExists #file_info
        (  (integer & notNullable) `as` #info_id
        :* ( vararray text & notNullable )`as` #filepath
        :* ( integer & notNullable ) `as` #size_bytes
        )
        (  primaryKey (#info_id :* #filepath) `as` #file_pk
        :* foreignKey #info_id #meta_info #info_id
            OnDeleteCascade OnUpdateNoAction `as` #file_fk
        ) >>>

    createTableIfNotExists #info_pieces
        (  (integer & notNullable) `as` #info_id
        :* (bytea & notNullable) `as` #pieces
        )
        (  check #pieces (length_ #pieces `rem_` 20 .== 0) `as` #pieces_divisible
        :* primaryKey #info_id `as` #pieces_pk
        :* foreignKey #info_id #meta_info #info_id
            OnDeleteCascade OnUpdateNoAction `as` #pieces_fk
        )

qInfoExists :: Query_ Schemas (Only ByteString) (Only Int32)
qInfoExists =
    select_
        (#info_id `as` #fromOnly)
        ( from (table (#meta_info `as` #t1))
        & where_ (#info_hash .== param @1)
        )

queryExists_ :: ByteString -> PoolPQ Schemas IO (Maybe (Only Int32))
queryExists_ infohash = do
    result <- (runQueryParams qInfoExists (Only infohash))
    firstRow result

queryExists :: ByteString -> PoolPQ Schemas IO Bool
queryExists infohash = queryExists_ infohash >>= return . isJust

{-
mIncrementScore :: Manipulation_ Schemas (ByteString, Double) ()
mIncrementScore =
    update_ #meta_info
        (Set (param @1) `as` #score)
        (#info_hash .== param @2)

incrementScore :: ByteString -> Double -> PoolPQ Schemas IO ()
incrementScore infohash addscore =
    manipulateParams_ mIncrementScore (infohash, addscore)
    -}

runSetup :: IO ()
runSetup = withConnection connstr $ define setup
