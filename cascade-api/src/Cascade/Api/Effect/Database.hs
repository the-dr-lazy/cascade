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
  , runInsert
  , runInsertReturningOne
  , runUpdateReturningOne
  , runDeleteReturningOne
  , withTransaction
  , postgresToFinal
  ) where

import qualified Control.Exception                  as X
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
import           Polysemy                            ( Member
                                                     , Sem
                                                     , makeSem
                                                     )
import           Polysemy.Final

data DatabaseL backend (m :: Type -> Type) a where
  RunSelectReturningList ::(BeamSqlBackend backend, Beam.FromBackendRow backend a) => Beam.SqlSelect backend a -> DatabaseL backend m [a]
  RunSelectReturningOne ::(BeamSqlBackend backend, Beam.FromBackendRow backend a) => Beam.SqlSelect backend a -> DatabaseL backend m (Maybe a)
  RunInsert ::BeamSqlBackend backend => Beam.SqlInsert backend table -> DatabaseL backend m ()
  RunInsertReturningOne ::(BeamSqlBackend backend, Beamable table, Beam.FromBackendRow backend (table Identity))=> Beam.SqlInsert backend table -> DatabaseL backend m (Maybe (table Identity))
  RunUpdateReturningOne ::(BeamSqlBackend backend, Beamable table, Beam.FromBackendRow backend (table Identity)) => Beam.SqlUpdate backend table -> DatabaseL backend m (Maybe (table Identity))
  RunDeleteReturningOne ::(BeamSqlBackend backend, Beamable table, Beam.FromBackendRow backend (table Identity)) => Beam.SqlDelete backend table -> DatabaseL backend m (Maybe (table Identity))
  WithTransaction ::m a -> DatabaseL backend m a

makeSem ''DatabaseL

postgresToFinal :: Member (Final IO) r => (forall x . (Postgres.Connection -> IO x) -> IO x) -> Sem (DatabaseL Beam.Postgres ': r) a -> Sem r a
postgresToFinal withConnection = interpretFinal \case
  RunSelectReturningList sql         -> Beam.runSelectReturningList sql |> runSql |> liftS
  RunSelectReturningOne  sql         -> Beam.runSelectReturningOne sql |> runSql |> liftS
  RunInsert              sql         -> Beam.runInsert sql |> runSql |> liftS
  RunInsertReturningOne  sql         -> Beam.runInsertReturningList sql |> runSql |> fmap listToMaybe |> liftS
  RunUpdateReturningOne  sql         -> Beam.runUpdateReturningList sql |> runSql |> fmap listToMaybe |> liftS
  RunDeleteReturningOne  sql         -> Beam.runDeleteReturningList sql |> runSql |> fmap listToMaybe |> liftS
  WithTransaction        transaction -> do
    transaction' <- runS transaction

    pure <| withConnection \connection ->
      X.bracket_ (Postgres.begin connection) (Postgres.rollback connection) (transaction' <* Postgres.commit connection)
 where
  runSql :: Pg a -> IO a
  runSql sql = withConnection (`runBeamPostgres` sql)
