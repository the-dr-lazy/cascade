{-|
Module      : Main
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Main (main) where

import qualified Cascade.Api
import qualified Data.Pool                          as Pool
import           Data.Pool                           ( Pool
                                                     , createPool
                                                     )
import qualified Database.PostgreSQL.Simple         as Postgres

mkDatabaseConnectionPool :: IO (Pool Postgres.Connection)
mkDatabaseConnectionPool = createPool acquire Postgres.close 1 10 10
  where acquire = Postgres.connectPostgreSQL "postgresql://cascade:cascade@postgresql:5432/cascade"

main :: IO ()
main = do
  databaseConnectionPool <- mkDatabaseConnectionPool
  Cascade.Api.main $ Pool.withResource databaseConnectionPool
