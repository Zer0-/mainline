SELECT
    parseText(name) AS name,
    concat('magnet:?xt=urn:btih:', encode(info_hash, 'hex')) AS magnet,
    score,
    added,
    pg_size_pretty(total_size) AS total_size,
    filecount
FROM meta_info WHERE info_hash = E'\\xINFO_HASH';
