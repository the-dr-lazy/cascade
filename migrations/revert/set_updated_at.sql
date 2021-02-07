-- Revert cascade:set_updated_at from pg

BEGIN;

DROP FUNCTION set_updated_at;

COMMIT;
