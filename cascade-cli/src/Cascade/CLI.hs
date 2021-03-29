module Cascade.CLI (cascadeCli) where

import qualified Cascade.Api
import qualified Data.Pool                          as Pool
import           Data.Pool                           ( Pool
                                                     , createPool
                                                     )
import qualified Database.PostgreSQL.Simple         as Postgres
import           Data.Version                        ( Version
                                                     , showVersion
                                                     )
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

mkDatabaseConnectionPool :: Options -> IO (Pool Postgres.Connection)
mkDatabaseConnectionPool Options {..} = do
  createPool acquire Postgres.close 1 10 10
 where
  acquire =
    Postgres.connectPostgreSQL
      $  "postgresql://"
      <> postgresUser
      <> ":"
      <> postgresPassword
      <> "@"
      <> postgresHost
      <> ":"
      <> show postgresPort
      <> "/"
      <> postgresDatabase

runCascadeApi :: Options -> IO ()
runCascadeApi options@Options {..} = do
  databaseConnectionPool <- mkDatabaseConnectionPool options
  Cascade.Api.main httpPort $ Pool.withResource databaseConnectionPool

cascadeVersion :: Version -> String
cascadeVersion version = intercalate "\n" [cVersion, cHash, cDate]
 where
  cVersion, cHash, cDate :: String
  cVersion = "Cascade CLI " <> "v" <> showVersion version
  cHash    = "Git revision: " <> $(gitHash)
  cDate    = "Last commit: " <> $(gitCommitDate)

versionP :: Version -> Parser (a -> a)
versionP version = infoOption (cascadeVersion version) $ long "version" <> short 'v' <> help "Show cascade's version"

data Options = Options
  { httpPort         :: !Int
  , postgresHost     :: !ByteString
  , postgresPort     :: !Int
  , postgresUser     :: !ByteString
  , postgresPassword :: !ByteString
  , postgresDatabase :: !ByteString
  }

cascadeP :: Parser Options
cascadeP =
  Options
    <$> option auto (long "http-port" <> metavar "INT" <> value 3141 <> showDefault <> help "Port number of Cascade Api")
    <*> strOption (long "postgres-host" <> metavar "STRING" <> value "localhost" <> showDefault <> help "Postgresql host")
    <*> option auto (long "postgres-port" <> metavar "INT" <> value 5432 <> showDefault <> help "Postgres port")
    <*> strOption (long "postgres-user" <> metavar "STRING" <> value "cascade" <> showDefault <> help "Postgresql user")
    <*> strOption (long "postgres-password" <> metavar "STRING" <> value "" <> showDefault <> help "Postgresql passowrd")
    <*> strOption (long "postgres-database" <> metavar "STRING" <> value "cascade-api" <> showDefault <> help "Postgresql database")

cliParser :: Version -> ParserInfo Options
cliParser version = info (helper <*> versionP version <*> cascadeP) $ fullDesc <> progDesc "Cascade Cli"

cascadeCli :: IO ()
cascadeCli = execParser (cliParser Meta.version) >>= runCascadeApi
