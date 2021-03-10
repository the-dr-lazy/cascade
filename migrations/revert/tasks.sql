-- Revert cascade:tasks from pg

BEGIN;

DROP TABLE tasks;

COMMIT;
