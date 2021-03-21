-- Revert cascade:user_projects from pg

BEGIN;

DROP TABLE user_projects;

COMMIT;
