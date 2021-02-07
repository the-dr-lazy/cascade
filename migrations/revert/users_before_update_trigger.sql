-- Revert cascade:users_before_update_trigger from pg

BEGIN;

DROP TRIGGER users_before_update_trigger ON users;

COMMIT;
