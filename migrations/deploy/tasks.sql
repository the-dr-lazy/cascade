-- Deploy cascade:tasks to pg
-- requires: uuid-ossp
-- requires: projects

BEGIN;

CREATE TABLE tasks (
  id          UUID DEFAULT uuid_generate_v4(),
  project_id  UUID,
  title       TEXT NOT NULL,
  deadline_at TIMESTAMP WITH TIME ZONE NOT NULL,

  CONSTRAINT tasks_pk PRIMARY KEY (id),

  CONSTRAINT tasks_project_id_fk FOREIGN KEY (project_id) REFERENCES projects (id) ON DELETE CASCADE
);

COMMIT;
