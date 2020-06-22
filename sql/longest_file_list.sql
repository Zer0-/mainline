/*
SELECT info_id, count(filepath) as filecount
FROM meta_info NATURAL JOIN file_info GROUP BY info_id
ORDER BY filecount DESC
LIMIT 100;
*/

/*
    -Now select to_tsvector(pathparts)
*/

BEGIN TRANSACTION;

CREATE AGGREGATE tsvector_agg(tsvector) (
   STYPE = pg_catalog.tsvector,
   SFUNC = pg_catalog.tsvector_concat,
   INITCOND = ''
);

-- ALTER TABLE meta_info ADD COLUMN search_index tsvector;
-- ALTER TABLE meta_info ADD COLUMN search_index tsvector DEFAULT NULL;
-- ALTER TABLE meta_info ALTER COLUMN search_index SET DEFAULT NULL;

-- ALTER TABLE meta_info ADD COLUMN filecount integer DEFAULT NULL;
-- ALTER TABLE meta_info DROP COLUMN total_size;
-- ALTER TABLE meta_info ADD COLUMN total_size bigint DEFAULT NULL;

UPDATE meta_info
SET search_index = tsvec
FROM (
    SELECT
        info_id,
        to_tsvector(
            'english',
            replace(
                (array_agg(parsed_name))[1],
                '.',
                ' '
            )
        ) || tsvector_agg(searchpart) AS tsvec
    FROM (

        SELECT
            info_id,
            parsetext(name) AS parsed_name,
            to_tsvector(
                'english',
                replace(
                    parsetext(unnest(filepath)),
                    '.',
                    ' '
                )
            ) AS searchpart
        FROM meta_info NATURAL JOIN file_info
        WHERE meta_info.search_index IS NULL

    ) AS t
    GROUP BY (info_id, searchpart)
) AS subquery
WHERE meta_info.info_id = subquery.info_id;

DROP AGGREGATE IF EXISTS tsvector_agg(tsvector);

--DROP INDEX IF EXISTS search_index_idx;
--CREATE INDEX search_index_idx ON meta_info USING GIN (search_index);

COMMIT;

BEGIN TRANSACTION;

-- Populate filecount, total_size columns

UPDATE meta_info
SET
    filecount = subquery.filecount,
    total_size = subquery.total_size
FROM (
    SELECT
        t.info_id,
        count(file_info.info_id) AS filecount,
        sum(file_info.size_bytes) AS total_size
    FROM (
        SELECT
            info_id
        FROM meta_info
        WHERE filecount IS NULL OR total_size IS NULL
        LIMIT 10000
    ) as t JOIN file_info ON t.info_id = file_info.info_id
    GROUP BY t.info_id
) as subquery
WHERE meta_info.info_id = subquery.info_id;

COMMIT;


-- BEGIN TRANSACTION;

-- CREATE INDEX score_idx ON meta_info USING btree (score);

-- DROP INDEX IF EXISTS file_info_idx;
-- CREATE INDEX file_info_idx ON file_info (info_id);

-- COMMIT;
