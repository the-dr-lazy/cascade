module Cascade.CLI (main) where

import qualified Cascade.Api
import qualified Data.Pool                          as Pool
import           Data.Pool                           ( Pool
                                                     , createPool
                                                     )
import qualified Database.PostgreSQL.Simple         as Postgres
import           Data.Version                        ( showVersion )
import qualified Paths_cascade_cli                  as Meta
                                                     ( version )
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
                                                     , showDefault
                                                     , value
                                                     , metavar
                                                     , auto
                                                     , option
                                                     , strOption
                                                     )
import           Development.GitRev                  ( gitCommitDate
                                                     , gitHash
                                                     )

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
  cDate    = "Last commit: " <> $(gitCommitDate)

versionP :: Parser (a -> a)
versionP = infoOption cascadeVersion <| mconcat [long "version", short 'v', help "Show cascade's version"]

data PostgresOptions = PostgresOptions
  { host     :: !String
  , port     :: !Word16
  , user     :: !String
  , password :: !String
  , database :: !String
  }

data Options = Options
  { httpPort        :: !Int
  , postgresOptions :: PostgresOptions
  }

postgresOptionsP :: Parser PostgresOptions
postgresOptionsP =
  PostgresOptions
    <$> strOption (mconcat [long "postgres-host", metavar "CASCADE_POSTGRES_HOST", value "localhost", showDefault, help "PostgreSQL host"])
    <*> option auto (mconcat [long "postgres-port", metavar "CASCADE_POSTGRES_PORT", value 5432, showDefault, help "PostgresSQL port"])
    <*> strOption (mconcat [long "postgres-user", metavar "CASCADE_POSTGRES_USER", value "cascade", showDefault, help "PostgreSQL user"])
    <*> strOption (mconcat [long "postgres-password", metavar "CASCADE_POSTGRES_PASSWORD", value "", showDefault, help "PostgreSQL passowrd"])
    <*> strOption
          (mconcat [long "postgres-database", metavar "CASCADE_POSTGRES_DATABASE", value "cascade-api", showDefault, help "PostgreSQL database"]
          )

optionsP :: Parser Options
optionsP =
  Options
    <$> option auto (mconcat [long "http-port", metavar "CASCADE_HTTP_PORT", value 3141, showDefault, help "Port number of Cascade Api"])
    <*> postgresOptionsP

cliParser :: ParserInfo Options
cliParser = info (helper <*> versionP <*> optionsP) <| fullDesc <> progDesc "Cascade Cli"

main :: IO ()
main = execParser cliParser >>= runCascadeApi
