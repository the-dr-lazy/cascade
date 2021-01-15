-- Deploy cascade:uuid-ossp to pg

BEGIN;

CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

COMMIT;
