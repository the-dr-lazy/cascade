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
import           Cascade.CLI.Data.Config             ( Config
                                                     , ConfigP(..)
                                                     , PostgresConfig
                                                     , PostgresConfigP(..)
                                                     , defaultPartialConfig
                                                     , finalise
                                                     )
import           Cascade.CLI.Data.Options            ( Options
                                                     , optionToPartialConfig
                                                     , optionsP
                                                     )
import           Cascade.CLI.Environment             ( readEnvPartialOptions )
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

mkDatabaseConnectionPool :: PostgresConfig -> IO (Pool Postgres.Connection)
mkDatabaseConnectionPool PostgresConfig {..} = do
  createPool acquire Postgres.close 1 10 10
 where
  connectionInfo =
    Postgres.ConnectInfo { connectHost = host, connectPort = port, connectUser = user, connectPassword = password, connectDatabase = database }
  acquire = Postgres.connect connectionInfo

runCascadeApi :: Config -> IO ()
runCascadeApi Config {..} = do
  databaseConnectionPool <- mkDatabaseConnectionPool postgresConfig
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

cliP :: ParserInfo Options
cliP = info (helper <*> versionP <*> optionsP) <| fullDesc <> progDesc "Cascade Cli"

getConfig :: IO Config
getConfig = do
  cliOptions <- execParser cliP
  let cliConfig = optionToPartialConfig cliOptions
  envConfig <- readEnvPartialOptions
  let combinedOptions = defaultPartialConfig <> envConfig <> cliConfig
  maybe (die "Couldn't make options") pure <| finalise combinedOptions

main :: IO ()
main = getConfig >>= runCascadeApi
