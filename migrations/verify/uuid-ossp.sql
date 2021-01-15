-- Verify cascade:uuid-ossp on pg

BEGIN;

DO $$
DECLARE
  count int;
BEGIN
  count := (SELECT COUNT(*)
            FROM pg_extension
            WHERE pg_extension.extname = 'uuid-ossp');

  ASSERT count = 1;
END $$;

ROLLBACK;
