-- Verify cascade:projects on pg

BEGIN;

SELECT id, name
FROM projects
WHERE FALSE;

ROLLBACK;
