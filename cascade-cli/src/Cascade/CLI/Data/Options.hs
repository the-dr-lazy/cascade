module Cascade.CLI.Data.Options (Options, optionsP, optionToPartialConfig) where

import           Options.Applicative                 ( Parser
                                                     , auto
                                                     , help
                                                     , long
                                                     , metavar
                                                     , option
                                                     , str
                                                     )

import           Cascade.CLI.Data.Config             ( ConfigP(..)
                                                     , PartialConfig
                                                     , PostgresConfigP(..)
                                                     , PostgresPartialConfig
                                                     )

data PostgresOptions = PostgresOptions
  { host     :: Maybe String
  , port     :: Maybe Word16
  , user     :: Maybe String
  , password :: Maybe String
  , database :: Maybe String
  }

data Options = Options
  { httpPort        :: Maybe Int
  , postgresOptions :: PostgresOptions
  }

postgresOptionsP :: Parser PostgresOptions
postgresOptionsP =
  PostgresOptions
    <$> optional (option str (mconcat [long "postgres-host", metavar "CASCADE_POSTGRES_HOST", help "PostgreSQL host"]))
    <*> optional (option auto (mconcat [long "postgres-port", metavar "CASCADE_POSTGRES_PORT", help "PostgresSQL port"]))
    <*> optional (option str (mconcat [long "postgres-user", metavar "CASCADE_POSTGRES_USER", help "PostgreSQL user"]))
    <*> optional (option str (mconcat [long "postgres-password", metavar "CASCADE_POSTGRES_PASSWORD", help "PostgreSQL passowrd"]))
    <*> optional (option str (mconcat [long "postgres-database", metavar "CASCADE_POSTGRES_DATABASE", help "PostgreSQL database"]))

optionsP :: Parser Options
optionsP =
  Options
    <$> optional (option auto (mconcat [long "http-port", metavar "CASCADE_HTTP_PORT", help "Port number of Cascade Api"]))
    <*> postgresOptionsP

postgresOptionsToPostgresPartialConfig :: PostgresOptions -> PostgresPartialConfig
postgresOptionsToPostgresPartialConfig PostgresOptions {..} =
  PostgresConfig { host = Last host, port = Last port, user = Last user, password = Last password, database = Last database }

optionToPartialConfig :: Options -> PartialConfig
optionToPartialConfig Options {..} =
  Config { httpPort = Last httpPort, postgresConfig = postgresOptionsToPostgresPartialConfig postgresOptions }
