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

import qualified Cascade.CLI.Data.Contract.Shell.Environment.Var
                                                    as Environment.Var
import qualified Cascade.CLI.Data.Model.Config      as Config
import           Cascade.CLI.Data.Model.Config       ( ConfigP(..) )
import qualified Cascade.CLI.Data.Model.Config.Default
                                                    as Config.Default
import           Data.Version                        ( showVersion )
import           Development.GitRev                  ( gitCommitDate
                                                     , gitHash
                                                     )
import           Options.Applicative                 ( Mod
                                                     , Parser
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

helpWithDefault :: String -> String -> Mod f a
helpWithDefault s def = help <| s <> " (default: " <> def <> ")"

postgresOptionsP :: Parser Postgres
postgresOptionsP = do
  host <-
    optional
    .  option str
    .  mconcat
    <| [long "postgres-host", metavar Environment.Var.cascadePostgresHost, helpWithDefault "PostgreSQL host" Config.Default.postgresHost]

  port <-
    optional
    .  option auto
    .  mconcat
    <| [ long "postgres-port"
       , metavar Environment.Var.cascadePostgresPort
       , helpWithDefault "PostgresSQL port" (show Config.Default.postgresPort)
       ]

  user <-
    optional
    .  option str
    .  mconcat
    <| [long "postgres-user", metavar Environment.Var.cascadePostgresUser, helpWithDefault "PostgreSQL user" Config.Default.postgresUser]

  password <-
    optional
    .  option str
    .  mconcat
    <| [ long "postgres-password"
       , metavar Environment.Var.cascadePostgresPassword
       , helpWithDefault "PostgreSQL passowrd" Config.Default.postgresPassword
       ]

  database <-
    optional
    .  option str
    .  mconcat
    <| [ long "postgres-database"
       , metavar Environment.Var.cascadePostgresDatabase
       , helpWithDefault "PostgreSQL database" Config.Default.postgresDatabase
       ]

  pure Postgres { .. }

optionsP :: Parser Options
optionsP = do
  httpPort <-
    optional
    .  option auto
    .  mconcat
    <| [long "http-port", metavar Environment.Var.cascadeHttpPort, helpWithDefault "Port number of Cascade Api" (show Config.Default.httpPort)]

  postgres <- postgresOptionsP

  pure Options { .. }

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
cliP = info (helper <*> versionP <*> optionsP) <| fullDesc <> progDesc "Cascade CLI"

toPartialConfig :: Options -> Config.Partial
toPartialConfig Options {..} = Config
  { httpPort = Last httpPort
  , postgres = let Postgres {..} = postgres
               in  Config.Postgres { host = Last host, port = Last port, user = Last user, password = Last password, database = Last database }
  }

readConfig :: IO Config.Partial
readConfig = toPartialConfig <$> execParser cliP
