{-# LANGUAGE
    DataKinds
  , TypeOperators
  , OverloadedLabels
  , OverloadedStrings
#-}

module Mainline.SQL
    ( Schemas
    , runSetup
    ) where

import Generics.SOP (NP (..))
import Data.ByteString (ByteString)
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
    )

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

connstr :: ByteString
connstr = "host=192.168.4.2 dbname=test user=guest password=invisiblegiraffe"

runSetup :: IO ()
runSetup = withConnection connstr $
    define setup


-- How to execute definitions using a Connection from Pool?
--
-- define :: Definition schemas0 schemas1 -> pq schemas0 schemas1 io ()
-- liftPQ :: (Connection -> IO a) -> pq q
--
-- if we use define we get a PQ
--
-- instance IndexedMonadTransPQ PQ where
--      define :: Definition schemas0 schemas1 -> PQ schemas0 schemas1 io ()
--
-- unPQ :: PQ schemas0 schemas1 io () -> K Connection schemas0 -> m (K x schemas1)
-- unK :: K a b -> a
--
-- need :: PQ schemas0 schemas1 io () -> Connection -> IO ()
-- need action con =
--      (((unPQ action) :: K Connection schemas0 -> m (K x schemas1)) (K con)) >>
--      return ()
