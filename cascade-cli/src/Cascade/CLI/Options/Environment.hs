{-|
Module      : Cascade.CLI.Options.Environment
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.CLI.Options.Environment (readEnvPartialOptions) where

import           Cascade.CLI.Options                 ( OptionsP(..)
                                                     , PartialOptions
                                                     , PostgresOptionsP(..)
                                                     , PostgresPartialOptions
                                                     )
import           System.Environment

readEnvMaybe :: Read a => String -> IO (Maybe a)
readEnvMaybe envName = do
  value <- lookupEnv envName
  pure <| value >>= readMaybe

readEnvPostgresPartialOptions :: IO PostgresPartialOptions
readEnvPostgresPartialOptions = do
  host     <- Last <$> lookupEnv "CASCADE_POSTGRES_HOST"
  port     <- Last <$> readEnvMaybe "CASCADE_POSTGRES_PORT"
  user     <- Last <$> lookupEnv "CASCADE_POSTGRES_USER"
  password <- Last <$> lookupEnv "CASCADE_POSTGRES_PASSWORD"
  database <- Last <$> lookupEnv "CASCADE_POSTGRES_DATABASE"
  pure PostgresOptions { .. }

readEnvPartialOptions :: IO PartialOptions
readEnvPartialOptions = do
  httpPort        <- Last <$> readEnvMaybe "CASCADE_HTTP_PORT"
  postgresOptions <- readEnvPostgresPartialOptions
  pure Options { .. }
