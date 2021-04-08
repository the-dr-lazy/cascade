{-|
Module      : Cascade.CLI.Data.Contract.Shell.Environment.Var
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

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
