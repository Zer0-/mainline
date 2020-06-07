SELECT * FROM (
    SELECT
        parseText(name) AS name,
        score,
        concat('magnet:?xt=urn:btih:', info_hash),
        added,
        info_id,
        encode(info_hash, 'hex'),
        ts_rank(search_index, query) AS sort_rank
    FROM meta_info, plainto_tsquery('english', '4k') query
    WHERE query @@ search_index
) AS t
ORDER BY sort_rank * log(score) DESC;
