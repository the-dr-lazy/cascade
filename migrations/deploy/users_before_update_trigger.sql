-- Deploy cascade:users_before_update_trigger to pg
-- requires: users
-- requires: set_updated_at

BEGIN;

DROP TRIGGER IF EXISTS users_before_update_trigger ON users;
CREATE TRIGGER users_before_update_trigger
BEFORE UPDATE ON users
FOR EACH ROW
EXECUTE PROCEDURE set_updated_at();

COMMIT;
