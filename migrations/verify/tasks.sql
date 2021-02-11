-- Verify cascade:tasks on pg

BEGIN;

SELECT id
FROM tasks
WHERE FALSE;

ROLLBACK;
