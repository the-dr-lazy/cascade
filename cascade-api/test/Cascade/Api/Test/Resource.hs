{-|
Module      : Cascade.Api.Test.Resource
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Test.Resource (withMigratedDatabaseConfig, withPostgresConnectionPool) where

import           Cascade.Api.Test.FilePath           ( findSqitchConfigFileUpward )
import qualified Control.Exception.Lifted           as X
import           Control.Lens                        ( _3
                                                     , view
                                                     )
import           Control.Monad.Base                  ( MonadBase )
import           Control.Monad.Managed
import qualified Data.Pool                          as Pool
import           Data.Pool                           ( Pool
                                                     , createPool
                                                     )
import qualified Database.PostgreSQL.Simple         as Postgres
import           Database.PostgreSQL.Simple          ( connectPostgreSQL )
import qualified Database.PostgreSQL.Simple.Options as Postgres
import qualified Database.Postgres.Temp             as TempPostgres
import qualified Relude.Unsafe                      as Unsafe
import           System.Directory                    ( getCurrentDirectory )
import           System.FilePath                     ( takeDirectory )
import           System.Process.Typed                ( nullStream
                                                     , runProcess_
                                                     , setStdout
                                                     , setWorkingDir
                                                     )
import qualified System.Process.Typed               as Process
import qualified Test.Tasty                         as Tasty
import           Test.Tasty                          ( TestTree )

migrate :: HasCallStack => TempPostgres.DB -> IO ()
migrate db = do
  cwd <- (getCurrentDirectory >>= findSqitchConfigFileUpward) |> (fmap . fmap) takeDirectory |> fmap Unsafe.fromJust
  Process.proc "sqitch" ["deploy", "--target", connectionUri . TempPostgres.toConnectionOptions $ db]
    |> setStdout nullStream
    |> setWorkingDir cwd
    |> runProcess_
 where
  connectionUri Postgres.Options {..} =
    "postgresql:///" <> Unsafe.fromJust (coerce dbname) <> "?host=" <> Unsafe.fromJust (coerce host) <> "&port=" <> show @String @Int
      (Unsafe.fromJust $ coerce port)

withMigratedDatabaseConfig :: (IO TempPostgres.Config -> TestTree) -> TestTree
withMigratedDatabaseConfig f =
  let acquire :: HasCallStack => IO (TempPostgres.Cache, TempPostgres.DB, TempPostgres.Snapshot)
      acquire = do
        cache          <- TempPostgres.setupInitDbCache TempPostgres.defaultCacheConfig
        migratedConfig <- throwE
          $ TempPostgres.cacheAction "~/.cascade/tmp-postgres" migrate (TempPostgres.defaultConfig <> TempPostgres.cacheConfig cache)
        db       <- throwE $ TempPostgres.startConfig migratedConfig
        snapshot <- throwE $ TempPostgres.takeSnapshot db
        pure (cache, db, snapshot)

      release :: (TempPostgres.Cache, TempPostgres.DB, TempPostgres.Snapshot) -> IO ()
      release (cache, db, snapshot) = do
        TempPostgres.cleanupSnapshot snapshot
        TempPostgres.stop db
        TempPostgres.cleanupInitDbCache cache
  in  Tasty.withResource acquire release $ f . fmap (TempPostgres.snapshotConfig . view _3)

withPostgresConnectionPool :: MonadManaged m => TempPostgres.Config -> m (Pool Postgres.Connection)
withPostgresConnectionPool config = do
  db <- managed $ throwE . TempPostgres.withConfig config
  liftIO $ createPool (connectPostgreSQL <| TempPostgres.toConnectionString db) Postgres.close 1 1 5

throwE :: (MonadBase IO m, Exception e) => m (Either e b) -> m b
throwE x = either X.throwIO pure =<< x
