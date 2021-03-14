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
                                                     , inspect
                                                     , interpretH
                                                     , makeSem
                                                     , raise
                                                     , runT
                                                     )
import           Polysemy.Final
import           Polysemy.Internal.Tactics           ( liftT )
import           Prelude                      hiding ( state )

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
postgresToFinal withConnection = interpretH \case
  RunSelectReturningList sql         -> Beam.runSelectReturningList sql |> runSql |> embedFinal |> liftT
  RunSelectReturningOne  sql         -> Beam.runSelectReturningOne sql |> runSql |> embedFinal |> liftT
  RunInsert              sql         -> Beam.runInsert sql |> runSql |> embedFinal |> liftT
  RunInsertReturningOne  sql         -> Beam.runInsertReturningList sql |> runSql |> fmap listToMaybe |> embedFinal |> liftT
  RunUpdateReturningOne  sql         -> Beam.runUpdateReturningList sql |> runSql |> fmap listToMaybe |> embedFinal |> liftT
  RunDeleteReturningOne  sql         -> Beam.runDeleteReturningList sql |> runSql |> fmap listToMaybe |> embedFinal |> liftT
  WithTransaction        transaction -> do
    transaction' <- runT transaction

    withWeavingToFinal \state weave _ -> do
      withConnection \connection ->
        let action = raise (postgresToFinal (apply connection) transaction') <$ state in Postgres.withTransaction connection . weave <| action

 where
  runSql :: Pg a -> IO a
  runSql sql = withConnection (`runBeamPostgres` sql)
