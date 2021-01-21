module Cascade.Api.Test.Resource
  ( withPostgresConnectionInAbortionBracket
  , withTemporaryPostgresConnectionPool
  ) where

import           Control.Exception.Lifted       ( bracket
                                                , throwIO
                                                )
import           Control.Lens                   ( _3
                                                , view
                                                )
import           Control.Monad.Managed
import qualified Data.Pool                     as Pool
import           Data.Pool                      ( Pool
                                                , createPool
                                                )
import qualified Database.PostgreSQL.Simple    as Postgres
import           Database.PostgreSQL.Simple     ( connectPostgreSQL )
import qualified Database.PostgreSQL.Simple.Options
                                               as Postgres
import qualified Database.Postgres.Temp        as TempPostgres
import qualified Relude.Unsafe                 as Unsafe
                                                ( fromJust )
import           System.Process.Typed           ( nullStream
                                                , runProcess_
                                                , setStdout
                                                )
import qualified System.Process.Typed          as Process
import qualified Test.Tasty                    as Tasty
import           Test.Tasty                     ( TestTree )

migrate :: TempPostgres.DB -> IO ()
migrate db =
  do
      Process.proc
        "sqitch"
        [ "deploy"
        , "--target"
        , connectionUri . TempPostgres.toConnectionOptions $ db
        ]
    |> setStdout nullStream
    |> runProcess_
 where
  connectionUri Postgres.Options {..} =
    "postgresql:///"
      <> Unsafe.fromJust (coerce dbname)
      <> "?host="
      <> Unsafe.fromJust (coerce host)
      <> "&port="
      <> show @String @Int (Unsafe.fromJust $ coerce port)

withTemporaryPostgresConnectionPool :: (  IO (Pool Postgres.Connection)
                                       -> TestTree
                                       )
                                    -> TestTree
withTemporaryPostgresConnectionPool f =
  let acquire :: IO
                   ( TempPostgres.Cache
                   , TempPostgres.DB
                   , Pool Postgres.Connection
                   )
      acquire = do
        cache <- TempPostgres.setupInitDbCache TempPostgres.defaultCacheConfig
        migratedConfig <- throwE $ TempPostgres.cacheAction
          "~/.cascade/tmp-postgres"
          migrate
          (TempPostgres.defaultConfig <> TempPostgres.cacheConfig cache)
        db   <- throwE $ TempPostgres.startConfig migratedConfig
        pool <- createPool
          (connectPostgreSQL $ TempPostgres.toConnectionString db)
          Postgres.close
          1
          1
          10
        pure (cache, db, pool)

      release :: (TempPostgres.Cache, TempPostgres.DB, Pool Postgres.Connection)
              -> IO ()
      release (cache, db, pool) = do
        Pool.destroyAllResources pool
        TempPostgres.stop db
        TempPostgres.cleanupInitDbCache cache
  in  Tasty.withResource acquire release $ f . fmap (view _3)
  where throwE x = either throwIO pure =<< x

withPostgresConnection :: MonadManaged m
                       => Pool Postgres.Connection
                       -> m Postgres.Connection
withPostgresConnection pool = managed $ Pool.withResource pool

withPostgresConnectionInAbortionBracket :: MonadManaged m
                                        => Pool Postgres.Connection
                                        -> m Postgres.Connection
withPostgresConnectionInAbortionBracket pool = do
  connection <- withPostgresConnection pool
  managed $ bracket (Postgres.begin connection)
                    (const $ Postgres.rollback connection)
  pure connection
