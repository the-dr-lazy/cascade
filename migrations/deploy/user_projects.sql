-- Deploy cascade:user_projects to pg
-- requires: users
-- requires: projects

BEGIN;

CREATE TABLE user_projects (
  user_id    UUID,
  project_id UUID,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),

  -- primary key
  CONSTRAINT user_projects_pk PRIMARY KEY (user_id, project_id),
  -- foreign key
  CONSTRAINT user_projects_user_id_fk FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE,
  CONSTRAINT user_projects_project_id_fk FOREIGN KEY (project_id) REFERENCES projects (id) ON DELETE CASCADE
);

COMMIT;
