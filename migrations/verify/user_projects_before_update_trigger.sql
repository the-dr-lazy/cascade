-- Verify cascade:user_projects_before_update_trigger on pg

BEGIN;

DO $$
DECLARE
  count int;
BEGIN
  count := (SELECT COUNT(*)
            FROM information_schema.triggers
            WHERE trigger_name = 'user_projects_before_update_trigger');

  ASSERT count = 1;
END $$;

ROLLBACK;
