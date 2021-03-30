{-|
Module      : Cascade.CLI.Options.CLI
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.CLI.Options.CLI (partialOptionsP) where

import           Cascade.CLI.Options                 ( OptionsP(..)
                                                     , PartialOptions
                                                     , PostgresOptionsP(..)
                                                     , PostgresPartialOptions
                                                     )
import           Options.Applicative                 ( Parser
                                                     , auto
                                                     , help
                                                     , long
                                                     , metavar
                                                     , option
                                                     , str
                                                     )

lastOption :: Parser a -> Parser (Last a)
lastOption = fmap Last . optional

postgresPartialOptionsP :: Parser PostgresPartialOptions
postgresPartialOptionsP =
  PostgresOptions
    <$> lastOption (option str (mconcat [long "postgres-host", metavar "CASCADE_POSTGRES_HOST", help "PostgreSQL host"]))
    <*> lastOption (option auto (mconcat [long "postgres-port", metavar "CASCADE_POSTGRES_PORT", help "PostgresSQL port"]))
    <*> lastOption (option str (mconcat [long "postgres-user", metavar "CASCADE_POSTGRES_USER", help "PostgreSQL user"]))
    <*> lastOption (option str (mconcat [long "postgres-password", metavar "CASCADE_POSTGRES_PASSWORD", help "PostgreSQL passowrd"]))
    <*> lastOption (option str (mconcat [long "postgres-database", metavar "CASCADE_POSTGRES_DATABASE", help "PostgreSQL database"]))

partialOptionsP :: Parser PartialOptions
partialOptionsP =
  Options
    <$> lastOption (option auto (mconcat [long "http-port", metavar "CASCADE_HTTP_PORT", help "Port number of Cascade Api"]))
    <*> postgresPartialOptionsP
