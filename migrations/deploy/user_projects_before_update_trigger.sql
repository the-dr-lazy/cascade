-- Deploy cascade:user_projects_before_update_trigger to pg
-- requires: user_projects
-- requires: set_updated_at

BEGIN;

DROP TRIGGER IF EXISTS user_projects_before_update_trigger ON user_projects;
CREATE TRIGGER user_projects_before_update_trigger
BEFORE UPDATE ON user_projects
FOR EACH ROW
EXECUTE PROCEDURE set_updated_at();

COMMIT;
