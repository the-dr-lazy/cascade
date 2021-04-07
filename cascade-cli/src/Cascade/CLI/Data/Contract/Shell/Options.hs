{-|
Module      : Cascade.CLI.Data.Contract.Shell.Options
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.CLI.Data.Contract.Shell.Options (readConfig) where

import qualified Cascade.CLI.Data.Model.Config      as Config
import           Cascade.CLI.Data.Model.Config       ( ConfigP(..) )
import           Data.Version                        ( showVersion )
import           Development.GitRev                  ( gitCommitDate
                                                     , gitHash
                                                     )
import           Options.Applicative                 ( Parser
                                                     , ParserInfo
                                                     , auto
                                                     , execParser
                                                     , fullDesc
                                                     , help
                                                     , helper
                                                     , info
                                                     , infoOption
                                                     , long
                                                     , metavar
                                                     , option
                                                     , progDesc
                                                     , short
                                                     , str
                                                     )
import qualified Paths_cascade_cli                  as Meta
                                                     ( version )

data Postgres = Postgres
  { host     :: Maybe String
  , port     :: Maybe Word16
  , user     :: Maybe String
  , password :: Maybe String
  , database :: Maybe String
  }

data Options = Options
  { httpPort :: Maybe Word16
  , postgres :: Postgres
  }

postgresOptionsP :: Parser Postgres
postgresOptionsP =
  Postgres
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

cascadeVersion :: String
cascadeVersion = intercalate "\n" [cVersion, cHash, cDate]
 where
  cVersion, cHash, cDate :: String
  cVersion = "Cascade CLI v" <> showVersion Meta.version
  cHash    = "Git revision: " <> $(gitHash)
  cDate    = "Last commit:  " <> $(gitCommitDate)

versionP :: Parser (a -> a)
versionP = infoOption cascadeVersion <| mconcat [long "version", short 'v', help "Show cascade's version"]

cliP :: ParserInfo Options
cliP = info (helper <*> versionP <*> optionsP) <| fullDesc <> progDesc "Cascade Cli"

toPostgresPartialConfig :: Postgres -> Config.PostgresPartial
toPostgresPartialConfig Postgres {..} =
  Config.Postgres { host = Last host, port = Last port, user = Last user, password = Last password, database = Last database }

toPartialConfig :: Options -> Config.Partial
toPartialConfig Options {..} = Config { httpPort = Last httpPort, postgres = toPostgresPartialConfig postgres }

readConfig :: IO Config.Partial
readConfig = toPartialConfig <$> execParser cliP
