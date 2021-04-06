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
import           Cascade.CLI.Data.Config             ( ConfigP(..)
                                                     , PostgresConfigP(..)
                                                     )
import qualified Cascade.CLI.Data.Config            as Config
import           Cascade.CLI.Data.Errors             ( Errors )
import qualified Cascade.CLI.Data.Model.FreePort    as FreePort
import qualified Cascade.CLI.Data.Options           as Options
import qualified Cascade.CLI.Environment            as Environment
import           Cascade.Data.Validation             ( Validation )
import qualified Cascade.Data.Validation            as Validation
import qualified Data.Pool                          as Pool
import           Data.Pool                           ( Pool
                                                     , createPool
                                                     )
import qualified Database.PostgreSQL.Simple         as Postgres

mkDatabaseConnectionPool :: Config.PostgresFinal -> IO (Pool Postgres.Connection)
mkDatabaseConnectionPool PostgresConfig {..} = do
  createPool acquire Postgres.close 1 10 10
 where
  connectionInfo =
    Postgres.ConnectInfo { connectHost = host, connectPort = port, connectUser = user, connectPassword = password, connectDatabase = database }
  acquire = Postgres.connect connectionInfo

runCascadeApi :: Config.Final -> IO ()
runCascadeApi Config {..} = do
  databaseConnectionPool <- mkDatabaseConnectionPool postgresConfig
  Cascade.Api.main Cascade.Api.Config { port = FreePort.un httpPort, withDatabaseConnection = Pool.withResource databaseConnectionPool }

getFinalConfig :: IO (Validation Errors Config.Final)
getFinalConfig = Config.finalize . fold =<< sequence [Environment.readConfig, Options.readConfig]

main :: IO ()
main = do
  vConfig <- getFinalConfig
  case vConfig of
    Validation.Failure _ -> putStrLn "Something went wrong!"
    Validation.Success a -> runCascadeApi a
