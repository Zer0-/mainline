/*
SELECT
    parseText(name) AS name,
    score,
    added,
    concat('magnet:?xt=urn:btih:', encode(info_hash, 'hex')) AS magnet,
    info_id,
    count(file_info.*) AS filecount,
    pg_size_pretty(sum(file_info.size_bytes)) AS total_size
FROM meta_info NATURAL JOIN file_info
GROUP BY (meta_info.name, score, added, info_hash, meta_info.info_id)
ORDER BY score DESC LIMIT 100;
*/

SELECT
    t.*,
    count(file_info.info_id) AS filecount,
    pg_size_pretty(sum(file_info.size_bytes)) AS total_size
FROM (
    SELECT
        parseText(name) AS name,
        score,
        added,
        concat('magnet:?xt=urn:btih:', encode(info_hash, 'hex')) AS magnet,
        info_id
    FROM meta_info
    ORDER BY score DESC
    LIMIT 100
) as t JOIN file_info ON t.info_id = file_info.info_id
GROUP BY (t.info_id, t.name, t.score, t.added, t.magnet)
ORDER BY score DESC;
