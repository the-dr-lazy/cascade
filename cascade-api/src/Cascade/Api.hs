module Cascade.Api
  ( main
  ) where

import qualified Cascade.Api.Effect.Database   as Database
import qualified Cascade.Api.Effect.Database.Project
                                               as Database.Project
import           Cascade.Api.Network.Wai.Application
import qualified Database.PostgreSQL.Simple    as Postgres
import qualified Network.Wai.Handler.Warp      as Warp
import           Polysemy                       ( runFinal )
import           Polysemy.Error                 ( errorToIOFinal )
import           Polysemy.Final                 ( embedToFinal )
import qualified Servant

main :: (forall a . (Postgres.Connection -> IO a) -> IO a) -> IO ()
main withDatabaseConnection = do
  Warp.run 3141 $ application
    ( Servant.Handler
    . ExceptT
    . runFinal
    . errorToIOFinal
    . embedToFinal
    . Database.runPostgres withDatabaseConnection
    . Database.Project.run
    )
