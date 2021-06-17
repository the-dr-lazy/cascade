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

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cascade.Api.Test.Resource
    ( withMigratedDatabaseConfig
    , withPostgresConnectionPool
    ) where

import           Cascade.Api.Test.FilePath          (findSqitchConfigFileUpward)
import qualified Control.Exception.Lifted           as X
import           Control.Lens                       (_2, view)
import           Control.Monad.Base                 (MonadBase)
import           Control.Monad.Managed
import           Data.Pool                          (Pool, createPool)
import           Database.PostgreSQL.Simple         (connectPostgreSQL)
import qualified Database.PostgreSQL.Simple         as Postgres
import qualified Database.PostgreSQL.Simple.Options as Postgres
import qualified Database.Postgres.Temp             as TempPostgres
import qualified Relude.Unsafe                      as Unsafe
import           System.Directory                   (getCurrentDirectory)
import           System.FilePath                    (takeDirectory)
import           System.Process.Typed               (nullStream, runProcess_, setStdout,
                                                     setWorkingDir)
import qualified System.Process.Typed               as Process
import           Test.Tasty                         (TestTree)
import qualified Test.Tasty                         as Tasty

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
  let acquire :: HasCallStack => IO (TempPostgres.Cache, TempPostgres.Config)
      acquire = do
        cache  <- TempPostgres.setupInitDbCache TempPostgres.defaultCacheConfig
        config <- throwE
          $ TempPostgres.cacheAction "~/.cascade/tmp-postgres" migrate (TempPostgres.defaultConfig <> TempPostgres.cacheConfig cache)
        pure (cache, config)

      release :: (TempPostgres.Cache, TempPostgres.Config) -> IO ()
      release (cache, _) = do
        TempPostgres.cleanupInitDbCache cache
  in  Tasty.withResource acquire release $ f . fmap (view _2)

withPostgresConnectionPool :: HasCallStack => MonadManaged m => TempPostgres.Config -> m (Pool Postgres.Connection)
withPostgresConnectionPool config = do
  {- HACK: There is a known issue about temporary postgres database cleanup
           which occasionally encounters with exception. By catching the exception
           we only let the tests continue to run but the problem is still alive and the temporary
           database doesn't cleanup on the host machine.
           Please see https://github.com/jfischoff/tmp-postgres/issues/266 and https://github.com/jfischoff/tmp-postgres/issues/251
  -}
  db <- managed $ X.bracket (throwE $ TempPostgres.startConfig config) (\db -> TempPostgres.stop db `X.catch` const @_ @X.IOException mempty)
  liftIO $ createPool (connectPostgreSQL <| TempPostgres.toConnectionString db) Postgres.close 1 1 5

throwE :: HasCallStack => MonadBase IO m => Exception e => m (Either e b) -> m b
throwE x = either X.throwIO pure =<< x
