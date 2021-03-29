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
                                                     )
import           Development.GitRev                  ( gitCommitDate
                                                     , gitHash
                                                     )

mkDatabaseConnectionPool :: IO (Pool Postgres.Connection)
mkDatabaseConnectionPool = createPool acquire Postgres.close 1 10 10
  where acquire = Postgres.connectPostgreSQL "postgresql://cascade:cascade@postgresql:5432/cascade"

runCascadeApi :: Options -> IO ()
runCascadeApi Options {..} = do
  databaseConnectionPool <- mkDatabaseConnectionPool
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
  { httpPort :: !Int
  }

cascadeP :: Parser Options
cascadeP = Options <$> option auto (long "http-port" <> help "Port number of Cascade Api" <> showDefault <> value 3141 <> metavar "INT")

cliParser :: Version -> ParserInfo Options
cliParser version = info (helper <*> versionP version <*> cascadeP) $ fullDesc <> progDesc "Cascade Cli"

cascadeCli :: IO ()
cascadeCli = execParser (cliParser Meta.version) >>= runCascadeApi
