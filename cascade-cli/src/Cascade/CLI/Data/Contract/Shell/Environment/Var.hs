module Cascade.CLI.Data.Contract.Shell.Environment.Var
  ( cascadeHttpPort
  , cascadePostgresHost
  , cascadePostgresPort
  , cascadePostgresUser
  , cascadePostgresPassword
  , cascadePostgresDatabase
  ) where

cascadeHttpPort :: String
cascadeHttpPort = "CASCADE_HTTP_PORT"

cascadePostgresHost :: String
cascadePostgresHost = "CASCADE_POSTGRES_HOST"

cascadePostgresPort :: String
cascadePostgresPort = "CASCADE_POSTGRES_PORT"

cascadePostgresUser :: String
cascadePostgresUser = "CASCADE_POSTGRES_USER"

cascadePostgresPassword :: String
cascadePostgresPassword = "CASCADE_POSTGRES_PASSWORD"

cascadePostgresDatabase :: String
cascadePostgresDatabase = "CASCADE_POSTGRES_DATABASE"
