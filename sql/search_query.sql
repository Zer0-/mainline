--EXPLAIN ANALYZE
SELECT
    name,
    pg_size_pretty(sum(file_info.size_bytes)) AS total_size,
    count(file_info.*) AS filecount,
    score,
    added,
    concat('magnet:?xt=urn:btih:', info_hash) as magnet,
    t.info_id,
    sort_rank
FROM (
    SELECT
        info_id,
        encode(info_hash, 'hex') AS info_hash,
        ts_rank(search_index, q.query) AS sort_rank,
        search_index,
        q.query,
        parseText(name) AS name,
        score,
        added
    FROM meta_info,
      ( SELECT
          plainto_tsquery(
              'english',
              convert_from(E'\\xHEX_ENCODED' :: bytea, 'utf8')
          ) AS query
      ) AS q
    WHERE q.query @@ search_index
    ORDER BY info_id
) AS t JOIN file_info ON t.info_id = file_info.info_id
GROUP BY (t.info_id, t.info_hash, t.sort_rank, t.name, t.score, t.added)
ORDER BY sort_rank * log(score) DESC;

