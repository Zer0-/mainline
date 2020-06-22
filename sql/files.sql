SELECT
    array_to_json((SELECT array_agg(parsetext(pathpart)) FROM unnest(filepath) AS pathpart)) AS path,
    pg_size_pretty(size_bytes) AS filesize
FROM file_info NATURAL JOIN meta_info
WHERE info_hash = E'\\xINFO_HASH';
