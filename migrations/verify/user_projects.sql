-- Verify cascade:user_projects on pg

BEGIN;

SELECT user_id
FROM user_projects
WHERE FALSE;

ROLLBACK;
