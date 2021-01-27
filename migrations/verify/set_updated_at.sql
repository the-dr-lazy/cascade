-- Verify cascade:set_updated_at on pg

BEGIN;

SELECT has_function_privilege('set_updated_at()', 'execute');

ROLLBACK;
