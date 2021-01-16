module Cascade.Api
  ( main
  ) where

import qualified Cascade.Api.Effect.Database   as Database
import qualified Cascade.Api.Effect.Database.Project
                                               as Database.Project
import           Cascade.Api.Network.Wai.Application
import           Data.Pool                      ( Pool
                                                , createPool
                                                )
import qualified Database.PostgreSQL.Simple    as Postgres
import qualified Network.Wai.Handler.Warp      as Warp
import           Polysemy                       ( runFinal )
import           Polysemy.Error                 ( errorToIOFinal )
import           Polysemy.Final                 ( embedToFinal )
import           Polysemy.Input                 ( runInputConst )
import qualified Servant

mkDatabaseConnectionPool :: IO (Pool Postgres.Connection)
mkDatabaseConnectionPool = createPool acquire Postgres.close 1 10 10
 where
  acquire = Postgres.connectPostgreSQL
    "postgresql://cascade:cascade@postgresql:5432/cascade"

main :: IO ()
main = do
  databaseConnectionPool <- mkDatabaseConnectionPool
  Warp.run 3141 $ application
    ( Servant.Handler
    . ExceptT
    . runFinal
    . errorToIOFinal
    . embedToFinal
    . runInputConst databaseConnectionPool
    . Database.runPostgres Database.runSqlByPostgresPooled
    . Database.Project.run
    )
