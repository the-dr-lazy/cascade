{-|
Module      : Cascade.Api.Effect.Database
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Effect.Database
  ( DatabaseL
  , runSelectReturningList
  , runSelectReturningOne
  , runInsertReturningOne
  , runUpdateReturningOne
  , runDeleteReturningOne
  , runPostgres
  ) where

import qualified Database.Beam                 as Beam
import           Database.Beam                  ( Beamable )
import           Database.Beam.Backend          ( BeamSqlBackend )
import qualified Database.Beam.Backend.SQL.BeamExtensions
                                               as Beam
                                                ( runDeleteReturningList
                                                , runInsertReturningList
                                                , runUpdateReturningList
                                                )
import qualified Database.Beam.Postgres        as Beam
                                                ( Postgres )
import           Database.Beam.Postgres         ( Pg
                                                , runBeamPostgres
                                                )
import qualified Database.PostgreSQL.Simple    as Postgres
import           Polysemy                       ( Embed
                                                , Member
                                                , Sem
                                                , embed
                                                , interpret
                                                , makeSem
                                                )

data DatabaseL backend (m :: Type -> Type) a where
  RunSelectReturningList ::(BeamSqlBackend backend, Beam.FromBackendRow backend a) => Beam.SqlSelect backend a -> DatabaseL backend m [a]
  RunSelectReturningOne ::(BeamSqlBackend backend, Beam.FromBackendRow backend a) => Beam.SqlSelect backend a -> DatabaseL backend m (Maybe a)
  RunInsertReturningOne ::(BeamSqlBackend backend, Beamable table, Beam.FromBackendRow backend (table Identity))=> Beam.SqlInsert backend table -> DatabaseL backend m (Maybe (table Identity))
  RunUpdateReturningOne ::(BeamSqlBackend backend, Beamable table, Beam.FromBackendRow backend (table Identity)) => Beam.SqlUpdate backend table -> DatabaseL backend m (Maybe (table Identity))
  RunDeleteReturningOne ::(BeamSqlBackend backend, Beamable table, Beam.FromBackendRow backend (table Identity)) => Beam.SqlDelete backend table -> DatabaseL backend m (Maybe (table Identity))

makeSem ''DatabaseL

runPostgres :: Member (Embed IO) r
            => (forall b . (Postgres.Connection -> IO b) -> IO b)
            -> Sem (DatabaseL Beam.Postgres ': r) a
            -> Sem r a
runPostgres withConnection = interpret \case
  RunSelectReturningList sql -> Beam.runSelectReturningList sql |> runSql
  RunSelectReturningOne  sql -> Beam.runSelectReturningOne sql |> runSql
  RunInsertReturningOne sql ->
    Beam.runInsertReturningList sql |> runSql |> fmap listToMaybe
  RunUpdateReturningOne sql ->
    Beam.runUpdateReturningList sql |> runSql |> fmap listToMaybe
  RunDeleteReturningOne sql ->
    Beam.runDeleteReturningList sql |> runSql |> fmap listToMaybe
 where
  runSql :: Member (Embed IO) r => Pg a -> Sem r a
  runSql sql = embed $ withConnection (`runBeamPostgres` sql)
