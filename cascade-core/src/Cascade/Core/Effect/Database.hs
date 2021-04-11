{-|
Module      : Cascade.Core.Effect.Database
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Core.Effect.Database
  ( DatabaseL
  , runSelectReturningList
  , runSelectReturningOne
  , runInsert
  , runInsertReturningOne
  , runUpdateReturningOne
  , runDeleteReturningOne
  , withTransaction
  , postgresToFinal
  ) where

import qualified Database.Beam                      as Beam
import           Database.Beam                       ( Beamable )
import           Database.Beam.Backend               ( BeamSqlBackend )
import qualified Database.Beam.Backend.SQL.BeamExtensions
                                                    as Beam
                                                     ( runDeleteReturningList
                                                     , runInsertReturningList
                                                     , runUpdateReturningList
                                                     )
import qualified Database.Beam.Postgres             as Beam
                                                     ( Postgres )
import           Database.Beam.Postgres              ( Pg
                                                     , runBeamPostgres
                                                     )
import qualified Database.PostgreSQL.Simple         as Postgres
import           Polysemy
import           Polysemy.Final
import           Polysemy.Internal.Tactics
import           Prelude                      hiding ( state )

data DatabaseL backend (m :: Type -> Type) a where
  RunSelectReturningList ::(BeamSqlBackend backend, Beam.FromBackendRow backend a) => Beam.SqlSelect backend a -> DatabaseL backend m [a]
  RunSelectReturningOne  ::(BeamSqlBackend backend, Beam.FromBackendRow backend a) => Beam.SqlSelect backend a -> DatabaseL backend m (Maybe a)
  RunInsert              ::BeamSqlBackend backend => Beam.SqlInsert backend table -> DatabaseL backend m ()
  RunInsertReturningOne  ::(BeamSqlBackend backend, Beamable table, Beam.FromBackendRow backend (table Identity))=> Beam.SqlInsert backend table -> DatabaseL backend m (Maybe (table Identity))
  RunUpdateReturningOne  ::(BeamSqlBackend backend, Beamable table, Beam.FromBackendRow backend (table Identity)) => Beam.SqlUpdate backend table -> DatabaseL backend m (Maybe (table Identity))
  RunDeleteReturningOne  ::(BeamSqlBackend backend, Beamable table, Beam.FromBackendRow backend (table Identity)) => Beam.SqlDelete backend table -> DatabaseL backend m (Maybe (table Identity))
  WithTransaction        ::m a -> DatabaseL backend m a

makeSem ''DatabaseL

postgresToFinal :: Member (Final IO) r => (forall x . (Postgres.Connection -> IO x) -> IO x) -> Sem (DatabaseL Beam.Postgres ': r) a -> Sem r a
postgresToFinal withConnection = interpretH \case
  RunSelectReturningList sql         -> Beam.runSelectReturningList sql |> runSql
  RunSelectReturningOne  sql         -> Beam.runSelectReturningOne sql |> runSql
  RunInsert              sql         -> Beam.runInsert sql |> runSql
  RunInsertReturningOne  sql         -> Beam.runInsertReturningList sql |> runSql |> (<<$>>) listToMaybe
  RunUpdateReturningOne  sql         -> Beam.runUpdateReturningList sql |> runSql |> (<<$>>) listToMaybe
  RunDeleteReturningOne  sql         -> Beam.runDeleteReturningList sql |> runSql |> (<<$>>) listToMaybe
  WithTransaction        transaction -> do
    transaction' <- runT transaction

    withWeavingToFinal \state weave _ -> do
      withConnection \connection ->
        let action = raise (postgresToFinal (apply connection) transaction') <$ state in Postgres.withTransaction connection . weave <| action

 where
  runSql :: Member (Final IO) r => Functor f => Pg a -> Sem (WithTactics e f m r) (f a)
  runSql = liftT . embedFinal . withConnection . flip runBeamPostgres
