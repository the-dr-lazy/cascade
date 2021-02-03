-- Deploy cascade:users to pg
-- requires: uuid-ossp

BEGIN;

CREATE TABLE users (
  id                 UUID DEFAULT uuid_generate_v4(),
  username           TEXT NOT NULL,
  email_address      TEXT NOT NULL,
  encrypted_password BYTEA NOT NULL,
  created_at         TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
  updated_at         TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),

  -- primary key
  CONSTRAINT users_pk PRIMARY KEY (id),
  -- alternative key
  CONSTRAINT users_username_ak      UNIQUE (username),
  CONSTRAINT users_email_address_ak UNIQUE (email_address)
);

COMMIT;
