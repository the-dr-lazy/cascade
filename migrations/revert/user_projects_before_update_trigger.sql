-- Revert cascade:user_projects_before_update_trigger from pg

BEGIN;

DROP TRIGGER user_projects_before_update_trigger ON user_projects;

COMMIT;
