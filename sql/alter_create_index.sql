BEGIN TRANSACTION;

ALTER TABLE meta_info ADD COLUMN search_index tsvector;

UPDATE meta_info
SET search_index = tsvec
FROM (
    SELECT
        info_id,
        --(array_agg(name))[1],
        --to_tsvector((array_agg(name))[1])
        to_tsvector('english', parsetext((array_agg(name))[1]))
        || to_tsvector('english', replace(parsetext((array_agg(name))[1]), '.', ' '))
        || to_tsvector('english', array_to_string(array_agg(pathpart), ' '))
        --|| to_tsvector('english', array_to_string(regexp_split_to_array(array_to_string(array_agg(pathpart), ' '), '\.'), ' '))
        AS tsvec
    FROM
    (
        SELECT info_id, name, parsetext(unnest(filepath)) as pathpart
        FROM meta_info NATURAL JOIN file_info
        GROUP BY (info_id, pathpart)
        ORDER BY info_id
    ) as t
    GROUP BY info_id
) as subquery
WHERE meta_info.info_id = subquery.info_id;

ROLLBACK;


-- query of what we're giving to to_tsvector
-- why doesn't text search split up periods? Is there any other processing we need to apply?
-- how does this groupby (info_id, pathpart) statement work, and how does unnest work?
