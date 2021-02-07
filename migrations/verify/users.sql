-- Verify cascade:users on pg

BEGIN;

SELECT id
FROM users
WHERE FALSE;

ROLLBACK;
