{-# LANGUAGE PartialTypeSignatures #-}

module Cascade.Effect.Database.Project
  ( ProjectL(..)
  , findAll
  , findById
  , create
  , updateById
  , deleteById
  , run
  ) where

import           Cascade.Data.Api.Project
import qualified Cascade.Data.Api.Project      as Project
import           Cascade.Database.Project       ( ProjectTable )
import qualified Cascade.Database.Project      as Database.Project
import qualified Cascade.Effect.Database       as Database
import           Cascade.Effect.Database        ( DatabaseL )
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

data ProjectL m a where
  FindAll ::ProjectL m [Readable Project]
  FindById ::Project.Id -> ProjectL m (Maybe (Readable Project))
  Create ::[Creatable Project] -> ProjectL m [Readable Project]
  UpdateById ::Project.Id -> Updatable Project -> ProjectL m (Maybe (Readable Project))
  DeleteById ::Project.Id -> ProjectL m (Maybe (Readable Project))

makeSem ''ProjectL

run :: forall backend r a
     . BeamSqlBackend backend
    => Beam.HasQBuilder backend
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
    Database.lookup #projects (Database.Project.PrimaryKey id)
      |> Database.runSelectReturningOne
      |> (fmap . fmap) toReadableProject
  Create creatables ->
    insertExpressions (fmap fromCreatableProject creatables)
      |> Database.insert #projects
      |> Database.runInsertReturningList
      |> (fmap . fmap) toReadableProject
  UpdateById id updatable ->
    Database.update #projects
                    (fromUpdatableProject updatable)
                    (\project -> project ^. #id ==. val_ id)
      |> Database.runUpdateReturningOne
      |> (fmap . fmap) toReadableProject
  DeleteById id ->
    Database.delete #projects (\project -> project ^. #id ==. val_ id)
      |> Database.runDeleteReturningOne
      |> (fmap . fmap) toReadableProject

toReadableProject :: Database.Project.Row -> Readable Project
toReadableProject Database.Project.Row {..} = ProjectR { .. }

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
