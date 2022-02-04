{-|
Module      : Cascade.CLI
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2022 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.CLI
    ( main
    ) where

import qualified Cascade.Api
import qualified Cascade.CLI.Data.Contract.Shell.Environment as Environment
import qualified Cascade.CLI.Data.Contract.Shell.Options     as Options
import           Cascade.CLI.Data.Model.Config               ( ConfigP (..) )
import qualified Cascade.CLI.Data.Model.Config               as Config
import qualified Cascade.CLI.Data.Model.FreePort             as FreePort
import           Cascade.Data.Validation                     ( Validation )
import qualified Cascade.Data.Validation                     as Validation
import qualified Cascade.Logger                              as Logger
import           Cascade.Logger.Message                      ( Scope (..) )
import qualified Cascade.Logger.Message                      as Logger.Message
import           Colog                                       ( usingLoggerT )
import           Data.Pool                                   ( Pool, createPool )
import qualified Data.Pool                                   as Pool
import qualified Database.PostgreSQL.Simple                  as Postgres

mkDatabaseConnectionPool :: Config.PostgresFinal -> IO (Pool Postgres.Connection)
mkDatabaseConnectionPool Config.Postgres {..} = do
  createPool acquire Postgres.close 1 10 10
 where
  connectionInfo =
    Postgres.ConnectInfo { connectHost = host, connectPort = port, connectUser = user, connectPassword = password, connectDatabase = database }
  acquire = Postgres.connect connectionInfo

runCascadeApi :: Config.Final -> IO ()
runCascadeApi Config {..} = do
  databaseConnectionPool <- mkDatabaseConnectionPool postgres
  Cascade.Api.main Cascade.Api.Config { port                   = FreePort.un httpPort
                                      , withDatabaseConnection = Pool.withResource databaseConnectionPool
                                      , logAction              = Logger.Message.Scoped Api >$< Logger.logScopedMessageToStdStreams
                                      }

getFinalConfig :: IO (Validation Config.Errors Config.Final)
getFinalConfig = Config.finalize . fold =<< sequence [Environment.readConfig, Options.readConfig]

main :: IO ()
main = usingLoggerT (Logger.Message.Scoped Cli >$< Logger.logScopedMessageToStdStreams) <| do
  vConfig <- lift getFinalConfig
  Logger.info "Configurations has been loaded."
  case vConfig of
    Validation.Failure errors -> mapM_ (Logger.error . Config.prettyPrintError) errors
    Validation.Success config -> lift <| runCascadeApi config
