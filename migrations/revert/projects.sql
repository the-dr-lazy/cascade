-- Revert cascade:projects from pg

BEGIN;

DROP TABLE projects;

COMMIT;
