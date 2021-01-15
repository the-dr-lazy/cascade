-- Revert cascade:uuid-ossp from pg

BEGIN;

DROP EXTENSION "uuid-ossp";

COMMIT;
