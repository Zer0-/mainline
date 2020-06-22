SELECT
    replace(parseText(name), '|', '\|') AS name,
    score,
    added,
    concat('magnet:?xt=urn:btih:', encode(info_hash, 'hex')) as magnet,
    filecount,
    pg_size_pretty(total_size) AS total_size
FROM meta_info
ORDER BY score DESC
LIMIT 100;
