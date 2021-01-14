{-|
Module      : Cascade.Api.Effect.Database.Project
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Effect.Database.Project
  ( ProjectL(..)
  , findAll
  , findById
  , create
  , updateById
  , deleteById
  , run
  ) where

import           Cascade.Api.Data.Project
import qualified Cascade.Api.Data.Project      as Project
import           Cascade.Api.Data.WrappedC
import           Cascade.Api.Database.Project   ( ProjectTable )
import qualified Cascade.Api.Database.Project  as Database.Project
import qualified Cascade.Api.Effect.Database   as Database
import           Cascade.Api.Effect.Database    ( DatabaseL )
import           Control.Lens                   ( (^.) )
import           Database.Beam                  ( (<-.)
                                                , (==.)
                                                , default_
                                                , insertExpressions
                                                , select
                                                , val_
                                                )
import qualified Database.Beam                 as Beam
import           Database.Beam.Backend          ( BeamSqlBackend
                                                , BeamSqlBackendCanSerialize
                                                )
import           Polysemy                       ( Member
                                                , Sem
                                                , interpret
                                                , makeSem
                                                )
import qualified Relude.Unsafe                 as Unsafe
                                                ( fromJust )

data ProjectL m a where
  FindAll    ::ProjectL m [Readable Project]
  FindById   ::Project.Id -> ProjectL m (Maybe (Readable Project))
  Create     ::Creatable Project -> ProjectL m (Readable Project)
  UpdateById ::Project.Id -> Updatable Project -> ProjectL m (Maybe (Readable Project))
  DeleteById ::Project.Id -> ProjectL m (Maybe (Readable Project))

makeSem ''ProjectL

run :: forall backend r a
     . BeamSqlBackend backend
    => Beam.HasQBuilder backend
    => Database.TableFieldsFulfillConstraint
         (Beam.FromBackendRow backend)
         ProjectTable
    => Database.TableFieldsFulfillConstraint
         (Beam.HasSqlEqualityCheck backend)
         ProjectTable
    => Database.TableFieldsFulfillConstraint
         (BeamSqlBackendCanSerialize backend)
         ProjectTable
    => Member (DatabaseL backend) r
    => Sem (ProjectL ': r) a
    -> Sem r a
run = interpret \case
  FindAll ->
    Database.all #projects
      |> select
      |> Database.runSelectReturningList
      |> (fmap . fmap) toReadableProject
  FindById id ->
    Database.lookup #projects (Database.Project.PrimaryKey $ coerce id)
      |> Database.runSelectReturningOne
      |> (fmap . fmap) toReadableProject
  Create creatable ->
    insertExpressions [fromCreatableProject creatable]
      |> Database.insert #projects
      |> Database.runInsertReturningOne
      -- Only @Just@ is acceptable.
      |> fmap Unsafe.fromJust
      |> fmap toReadableProject
  UpdateById id updatable -> case updatable of
    ProjectU { name = Nothing } -> run $ findById id
    _ ->
      Database.update #projects
                      (fromUpdatableProject updatable)
                      (\project -> project ^. #id ==. val_ (coerce id))
        |> Database.runUpdateReturningOne
        |> (fmap . fmap) toReadableProject
  DeleteById id ->
    Database.delete #projects (\project -> project ^. #id ==. val_ (coerce id))
      |> Database.runDeleteReturningOne
      |> (fmap . fmap) toReadableProject

toReadableProject :: Database.Project.Row -> Readable Project
toReadableProject Database.Project.Row {..} = ProjectR { id = coerce id, name }

fromCreatableProject :: BeamSqlBackend backend
                     => Database.TableFieldsFulfillConstraint
                          (BeamSqlBackendCanSerialize backend)
                          ProjectTable
                     => Creatable Project
                     -> ProjectTable (Beam.QExpr backend s)
fromCreatableProject ProjectC {..} =
  Database.Project.Row { id = default_, name = val_ name }

fromUpdatableProject :: BeamSqlBackend backend
                     => Database.TableFieldsFulfillConstraint
                          (BeamSqlBackendCanSerialize backend)
                          ProjectTable
                     => Updatable Project
                     -> (  forall s
                         . ProjectTable (Beam.QField s)
                        -> Beam.QAssignment backend s
                        )
fromUpdatableProject updatable project =
  mconcat . catMaybes $ [(project ^. #name <-.) . val_ <$> updatable ^. #name]
