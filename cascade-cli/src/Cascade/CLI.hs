{-|
Module      : Cascade.CLI
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.CLI (main) where

import qualified Cascade.Api
import           Cascade.CLI.Options                 ( Options
                                                     , OptionsP(..)
                                                     , PartialOptions
                                                     , PostgresOptions
                                                     , PostgresOptionsP(..)
                                                     , mkOptions
                                                     )
import           Cascade.CLI.Options.CLI             ( partialOptionsP )
import           Cascade.CLI.Options.Default         ( defaultPartialOptions )
import           Cascade.CLI.Options.Environment     ( readEnvPartialOptions )
import qualified Data.Pool                          as Pool
import           Data.Pool                           ( Pool
                                                     , createPool
                                                     )
import           Data.Version                        ( showVersion )
import qualified Database.PostgreSQL.Simple         as Postgres
import           Development.GitRev                  ( gitCommitDate
                                                     , gitHash
                                                     )
import           Options.Applicative                 ( Parser
                                                     , ParserInfo
                                                     , execParser
                                                     , fullDesc
                                                     , help
                                                     , helper
                                                     , info
                                                     , infoOption
                                                     , long
                                                     , progDesc
                                                     , short
                                                     )
import qualified Paths_cascade_cli                  as Meta
                                                     ( version )

mkDatabaseConnectionPool :: PostgresOptions -> IO (Pool Postgres.Connection)
mkDatabaseConnectionPool PostgresOptions {..} = do
  createPool acquire Postgres.close 1 10 10
 where
  connectionInfo =
    Postgres.ConnectInfo { connectHost = host, connectPort = port, connectUser = user, connectPassword = password, connectDatabase = database }
  acquire = Postgres.connect connectionInfo

runCascadeApi :: Options -> IO ()
runCascadeApi Options {..} = do
  databaseConnectionPool <- mkDatabaseConnectionPool postgresOptions
  Cascade.Api.main Cascade.Api.Config { port = httpPort, withDatabaseConnection = Pool.withResource databaseConnectionPool }

cascadeVersion :: String
cascadeVersion = intercalate "\n" [cVersion, cHash, cDate]
 where
  cVersion, cHash, cDate :: String
  cVersion = "Cascade CLI v" <> showVersion Meta.version
  cHash    = "Git revision: " <> $(gitHash)
  cDate    = "Last commit:  " <> $(gitCommitDate)

versionP :: Parser (a -> a)
versionP = infoOption cascadeVersion <| mconcat [long "version", short 'v', help "Show cascade's version"]

cliP :: ParserInfo PartialOptions
cliP = info (helper <*> versionP <*> partialOptionsP) <| fullDesc <> progDesc "Cascade Cli"

getOptions :: IO Options
getOptions = do
  cliOptions <- execParser cliP
  envOptions <- readEnvPartialOptions
  let combinedOptions = defaultPartialOptions <> envOptions <> cliOptions
  maybe (die "Couldn't make options") pure <| mkOptions combinedOptions

main :: IO ()
main = getOptions >>= runCascadeApi
