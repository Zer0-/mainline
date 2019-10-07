BEGIN TRANSACTION;

CREATE TABLE IF NOT EXISTS meta_info
    ( info_id serial
    , info_hash bytea NOT NULL
    , added timestamp with time zone NOT NULL DEFAULT current_timestamp
    , score double precision NOT NULL DEFAULT 0
    , CONSTRAINT hash_len CHECK (length(info_hash) = 20)
    , CONSTRAINT info_pk PRIMARY KEY (info_id)
    , CONSTRAINT info_hash_unique UNIQUE (info_hash)
    );

CREATE TABLE IF NOT EXISTS file_info
    ( info_id integer
    , filepath text[] NOT NULL
    , size_bytes integer NOT NULL
    , CONSTRAINT file_pk PRIMARY KEY (info_id, filepath)
    , CONSTRAINT file_fk FOREIGN KEY (info_id) REFERENCES meta_info (info_id) ON DELETE CASCADE
    );

CREATE TABLE IF NOT EXISTS info_pieces
    ( info_id integer
    , pieces bytea NOT NULL
    , CONSTRAINT pieces_divisible CHECK (length(pieces) % 20 = 0)
    , CONSTRAINT pieces_pk PRIMARY KEY (info_id)
    , CONSTRAINT pieces_fk FOREIGN KEY (info_id) REFERENCES meta_info (info_id) ON DELETE CASCADE
    );

DROP TABLE IF EXISTS file_info;
DROP TABLE IF EXISTS info_pieces;
DROP TABLE IF EXISTS meta_info;

ROLLBACK;
