-- 18, SP-GIST, box_ops, @> -- Sebastian Kondraciuk, 298451

-- Seed database
DROP TABLE IF EXISTS boxes;
CREATE TABLE boxes(b BOX);
INSERT INTO boxes(b) SELECT box(point(n, 1), point(n, 0)) FROM generate_series(1, 100000) as n;

-- Experiment start
EXPLAIN ANALYZE SELECT * FROM boxes WHERE b @> point('100.5,0.5');
VACUUM ANALYZE boxes;
EXPLAIN ANALYZE SELECT * FROM boxes WHERE b @> box '((0.5,0.5),(0.5,0.5))';

CREATE INDEX ON boxes USING SPGIST(b box_ops);

VACUUM ANALYZE boxes;
-- Barely any improvement
EXPLAIN ANALYZE SELECT * FROM boxes WHERE b @> point('100.5,0.5');

VACUUM ANALYZE boxes;
-- From ~19ms to ~0.065ms
EXPLAIN ANALYZE SELECT * FROM boxes WHERE b @> box '((0.5,0.5),(0.5,0.5))';


-- Output:
--                                                       QUERY PLAN
-- ----------------------------------------------------------------------------------------------------------------------
--  Seq Scan on boxes  (cost=10000000000.00..10000001987.20 rows=100 width=32) (actual time=9.865..9.865 rows=0 loops=1)
--    Filter: (b @> '(100.5,0.5)'::point)
--    Rows Removed by Filter: 100000
--  Planning Time: 17.464 ms
--  Execution Time: 21.896 ms
-- (5 rows)


-- VACUUM
--                                                       QUERY PLAN
-- ----------------------------------------------------------------------------------------------------------------------
--  Seq Scan on boxes  (cost=10000000000.00..10000001986.00 rows=100 width=32) (actual time=7.332..7.333 rows=0 loops=1)
--    Filter: (b @> '(0.5,0.5),(0.5,0.5)'::box)
--    Rows Removed by Filter: 100000
--  Planning Time: 0.065 ms
--  Execution Time: 7.347 ms
-- (5 rows)


-- CREATE INDEX
-- VACUUM
--                                                            QUERY PLAN
-- --------------------------------------------------------------------------------------------------------------------------------
--  Index Only Scan using boxes_b_idx on boxes  (cost=0.28..6934.28 rows=100 width=32) (actual time=18.025..18.025 rows=0 loops=1)
--    Filter: (b @> '(100.5,0.5)'::point)
--    Rows Removed by Filter: 100000
--    Heap Fetches: 0
--  Planning Time: 29.006 ms
--  Execution Time: 18.798 ms
-- (6 rows)


-- VACUUM
--                                                          QUERY PLAN
-- ----------------------------------------------------------------------------------------------------------------------------
--  Index Only Scan using boxes_b_idx on boxes  (cost=0.28..10.03 rows=100 width=32) (actual time=0.024..0.024 rows=0 loops=1)
--    Index Cond: (b @> '(0.5,0.5),(0.5,0.5)'::box)
--    Heap Fetches: 0
--  Planning Time: 15.690 ms
--  Execution Time: 0.065 ms
-- (5 rows)
