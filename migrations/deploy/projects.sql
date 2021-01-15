-- Deploy cascade:projects to pg
-- requires: uuid-ossp

BEGIN;

CREATE TABLE projects (
  id   UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  name TEXT NOT NULL
);

COMMIT;
