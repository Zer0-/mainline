SELECT
    replace(parseText(name), '|', '\|') AS name,
    pg_size_pretty(total_size) as total_size,
    filecount,
    score,
    added,
    concat('magnet:?xt=urn:btih:', encode(info_hash, 'hex')) as magnet,
    ts_rank(search_index, query) AS sort_rank
FROM
    meta_info,
    plainto_tsquery(
        'english',
        convert_from(E'\\xHEX_ENCODED' :: bytea, 'utf8')
    ) query
WHERE query @@ search_index
ORDER BY score DESC;
