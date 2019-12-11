{-# LANGUAGE
    DataKinds
  , TypeOperators
  , OverloadedLabels
  , OverloadedStrings
  , TypeApplications
  , PatternSynonyms
  , FlexibleContexts
#-}

module Mainline.SQL
    ( Schemas
    , runSetup
    , connstr
    , queryExists
    , insertInfo
    , incrementScore
    , mIncrementScore
    ) where

import Generics.SOP (NP (..))
import Data.Int (Int32)
import Data.Maybe (isJust)
import Data.ByteString (ByteString)
import Squeal.PostgreSQL
    ( Public
    , PQ
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
    , Optional (..)
    , Manipulation
    , Manipulation_
    , insertInto_
    , insertInto
    , QueryClause (..)
    , Aliased
    , Grouping (..)
    , Expression
    , literal
    , array
    , pattern Values_
    , ConflictClause (..)
    , ReturningClause (..)
    , with
    , (!)
    , pattern Values_
    , common
    , manipulateParams
    , transactionally_
    , manipulate_
    , manipulateParams_
    , update_
    )

connstr :: ByteString
connstr = "host=192.168.4.2 dbname=test user=guest password=invisiblegiraffe"

type Schemas = Public Schema

type Schema =
   '[ "meta_info"   ::: 'Table (MetaInfoConstraints   :=> MetaInfoColumns)
    , "file_info"   ::: 'Table (FileInfoConstraints   :=> FileInfoColumns)
    , "info_pieces" ::: 'Table (InfoPiecesConstraints :=> InfoPiecesColumns)
    ]

type MetaInfoConstraints =
   '[ "hash_len"         ::: 'Check      '["info_hash"]
    , "info_pk"          ::: 'PrimaryKey '["info_id"]
    , "info_hash_unique" ::: 'Unique     '["info_hash"]
    ]

type MetaInfoColumns =
   '[ "info_id"         ::: 'Def   :=> 'NotNull 'PGint4
    , "info_hash"       ::: 'NoDef :=> 'NotNull 'PGbytea
    , "piece_len_bytes" ::: 'NoDef :=> 'NotNull 'PGint4
    , "name"            ::: 'NoDef :=> 'NotNull 'PGbytea
    , "added"           ::: 'Def   :=> 'NotNull 'PGtimestamptz
    , "score"           ::: 'Def   :=> 'NotNull 'PGfloat8
    ]

type FileInfoConstraints =
   '[ "file_pk" ::: 'PrimaryKey '["info_id", "filepath"]
    , "file_fk" ::: 'ForeignKey '["info_id"] "meta_info" '["info_id"]
    ]

type FileInfoColumns =
   '[ "info_id"    ::: 'NoDef :=> 'NotNull 'PGint4
    , "filepath"   ::: 'NoDef :=> 'NotNull ('PGvararray ('NotNull 'PGbytea))
    , "size_bytes" ::: 'NoDef :=> 'NotNull 'PGint4
    ]

type InfoPiecesConstraints =
   '[ "pieces_divisible" ::: 'Check '["pieces"]
    , "pieces_pk" ::: 'PrimaryKey '["info_id"]
    , "pieces_fk" ::: 'ForeignKey '["info_id"] "meta_info" '["info_id"]
    ]

type InfoPiecesColumns =
   '[ "info_id" ::: 'NoDef :=> 'NotNull 'PGint4
    , "pieces"  ::: 'NoDef :=> 'NotNull 'PGbytea
    ]

setup :: Definition Schemas Schemas
setup =
    createTableIfNotExists #meta_info
        (  serial `as` #info_id
        :* ( bytea & notNullable ) `as` #info_hash
        :* ( integer & notNullable ) `as` #piece_len_bytes
        :* ( text & notNullable ) `as` #name
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

queryExists_ :: ByteString -> PQ Schemas Schemas IO (Maybe (Only Int32))
queryExists_ infohash = do
    result <- runQueryParams qInfoExists (Only infohash)
    firstRow result

queryExists :: ByteString -> PQ Schemas Schemas IO Bool
queryExists infohash = queryExists_ infohash >>= return . isJust


mInsertMetaInfo
    :: Double
    -> Manipulation
        '[]
        Schemas
        InfoParamsType
        '["info_id" ::: 'NotNull 'PGint4]
mInsertMetaInfo i = insertInto #meta_info
    ( Values_
        ( Default `as` #info_id
        :* Set (param @1)  `as` #info_hash
        :* Set (param @2)  `as` #piece_len_bytes
        :* Set (param @3)  `as` #name
        :* Default         `as` #added
        :* Set (literal i) `as` #score
        )
    )
    OnConflictDoRaise
    (Returning (#info_id `as` #info_id))


type InfoParamsType =
   '[ 'NotNull 'PGbytea
    , 'NotNull 'PGint4
    , 'NotNull 'PGbytea
    , 'NotNull 'PGbytea
    ]

type FileVal = NP
    ( Aliased
        ( Optional
            ( Expression
                '[]
                '[]
                'Ungrouped
                Schemas
                '[]
                '[]
            )
        )
    )
    FileInfoColumns


mInsertFiles
    :: [(Int32, [ByteString], Int32)]
    -> Manipulation_ Schemas () ()
mInsertFiles haskValues =
    insertInto_ #file_info
        ( Values
            (mkRow $ head haskValues)
            (map mkRow $ drop 1 haskValues)
        )

    where
        mkRow :: (Int32, [ByteString], Int32) -> FileVal
        mkRow (iid, path, size) =
            (  Set (literal iid) `as` #info_id
            :* Set (array $ map literal path) `as` #filepath
            :* Set (literal size) `as` #size_bytes
            )

mInsertInfo
    :: Double
    -> Manipulation_ Schemas (ByteString, Int32, ByteString, ByteString) (Only Int32)
mInsertInfo score =
    with ((mInsertMetaInfo score) `as` #tmp_info_id) insPieces

    where
        insPieces = insertInto #info_pieces
            ( Select
                (  Set (#tmp_info_id ! #info_id) `as` #info_id
                :* Set (param @4) `as` #pieces
                )
                (from (common #tmp_info_id))
            )
            OnConflictDoRaise
            (Returning (#info_id `as` #fromOnly))

insertInfoA
    :: ByteString -- infohash
    -> Int32      -- piece_len_bytes
    -> ByteString -- name
    -> Double     -- score
    -> ByteString -- pieces hash blob
    -> PQ Schemas Schemas IO (Maybe (Only Int32))
insertInfoA infohash piecelen name score piecesBs = do
    result <- manipulateParams
        (mInsertInfo score)
        (infohash, piecelen, name, piecesBs)
    firstRow result

insertInfo
    :: ByteString -- infohash
    -> Int32      -- piece_len_bytes
    -> ByteString -- name
    -> Double     -- score
    -> ByteString -- pieces hash blob
    -> [([ByteString], Int32)] -- files (path, size)
    -> PQ Schemas Schemas IO ()
insertInfo infohash piecelen name score piecesBs filetups =
    transactionally_ $ do
        result <- insertInfoA infohash piecelen name score piecesBs

        case result of
            Nothing -> undefined
            Just infoid ->
                manipulate_ $
                    mInsertFiles
                        (map (\(a, b) -> (fromOnly infoid, a, b)) filetups)

mIncrementScore :: Manipulation_ Schemas (ByteString, Double) ()
mIncrementScore =
    update_ #meta_info
        (Set (#score + param @2) `as` #score)
        (#info_hash .== param @1)

incrementScore :: ByteString -> Double -> PQ Schemas Schemas IO ()
incrementScore infohash addscore =
    manipulateParams_ mIncrementScore (infohash, addscore)

runSetup :: IO ()
runSetup = withConnection connstr $ define setup
