SELECT
    name,
    pg_size_pretty(sum(file_info.size_bytes)) AS total_size,
    count(file_info.*) AS filecount,
    score,
    added,
    concat('magnet:?xt=urn:btih:', info_hash),
    t.info_id,
    sort_rank
FROM (
    SELECT
        info_id,
        encode(info_hash, 'hex') AS info_hash,
        ts_rank(search_index, query) AS sort_rank,
        parseText(name) AS name,
        score,
        added
    FROM meta_info, plainto_tsquery('english', 'yu yu hakusho') query
    WHERE query @@ search_index
) AS t JOIN file_info ON t.info_id = file_info.info_id
GROUP BY (t.info_id, t.info_hash, t.sort_rank, t.name, t.score, t.added)
ORDER BY sort_rank * log(score) DESC;

