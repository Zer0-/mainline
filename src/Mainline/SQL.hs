{-# LANGUAGE
    DataKinds
  , TypeOperators
#-}

module Mainline.SQL
    ( Schemas
    ) where

import Squeal.PostgreSQL
    ( Public
    , SchemumType (..)
    , (:::)
    , (:=>)
    , TableConstraint (..)
    , ColumnConstraint (..)
    , NullityType (..)
    , PGType (..)
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
    '[ "file_pk" ::: 'PrimaryKey '["info_id"]
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

-- How to execute definitions using a Connection from Pool?
-- define :: Definition schemas0 schemas1 -> pq schemas0 schemas1 io ()
-- liftPQ :: (Connection -> IO a) -> pq q
--
-- if we use define we get a PQ
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
-- need = ((unPQ `s` K) :: PQ schemas0 schemas1 io () -> Connection -> m (K x schemas1))
