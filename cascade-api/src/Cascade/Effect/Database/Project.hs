module Cascade.Effect.Database.Project
  ( ProjectL (..)
  , findAll
  , findById
  , create
  , updateById
  , deleteById
  , run
  ) where

import           Cascade.Database.Project       ( Project
                                                , ProjectTable
                                                )
import qualified Cascade.Database.Project      as Project
import qualified Cascade.Effect.Database       as Database
import           Cascade.Effect.Database        ( DatabaseL )
import           Control.Lens                   ( (^.) )
import           Database.Beam                  ( (==.)
                                                , PrimaryKey
                                                , insertValues
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
  FindAll ::ProjectL m [Project]
  FindById ::Project.Id -> ProjectL m (Maybe Project)
  Create ::[Project] -> ProjectL m [Project]
  UpdateById ::Project.Id
             -> (forall backend s
                . BeamSqlBackend backend
               => Database.TableFieldsFulfillConstraint (BeamSqlBackendCanSerialize backend) ProjectTable
               => ProjectTable (Beam.QField s)
               -> [Beam.QAssignment backend s]
                )
             -> ProjectL m (Maybe Project)
  DeleteById ::Project.Id -> ProjectL m ()

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
    Database.all #projects |> select |> Database.runSelectReturningList
  FindById id -> Database.lookup #projects id |> Database.runSelectReturningOne
  Create rows ->
    insertValues rows
      |> Database.insert #projects
      |> Database.runInsertReturningList
  UpdateById (Project.Id id) assignments ->
    Database.update #projects
                    (mconcat . assignments)
                    (\project -> project ^. #id ==. val_ id)
      |> Database.runUpdateReturningOne
  DeleteById (Project.Id id) ->
    Database.delete #projects (\project -> project ^. #id ==. val_ id)
      |> Database.runDelete
