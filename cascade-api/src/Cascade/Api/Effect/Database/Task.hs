module Cascade.Api.Effect.Database.Task
  ( TaskL(..)
  , findByProjectId
  , create
  , run
  ) where

import qualified Cascade.Api.Data.Project        as Project
import qualified Cascade.Api.Data.Task           as Task
import           Cascade.Api.Data.OffsetDatetime ( FormattedOffsetDatetime(..) )
import           Cascade.Api.Data.WrappedC
import           Cascade.Api.Database.Task      ( TaskTable )
import qualified Cascade.Api.Database.Task     as Database.Task
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
                                                , guard_
                                                )
import qualified Database.Beam                 as Beam
import           Database.Beam.Backend          ( BeamSqlBackend
                                                , BeamSqlBackendCanSerialize
                                                , BackendFromField
                                                , FromBackendRow
                                                )
import           Polysemy                       ( Member
                                                , Sem
                                                , interpret
                                                , makeSem
                                                )
import qualified Relude.Unsafe                 as Unsafe
                                                ( fromJust )
import           Chronos                        ( Time )

import           Cascade.Api.Database.Project   ( ProjectTable )
import           Database.Beam                  ( Beamable
                                                , PrimaryKey
                                                )


data TaskL m a where
  FindByProjectId :: Project.Id -> TaskL m [Task.Readable]
  Create          :: Task.Creatable -> Project.Id -> TaskL m Task.Readable

makeSem ''TaskL

run :: forall backend r a
     . BeamSqlBackend backend
    => Beam.HasQBuilder backend
    => Database.TableFieldsFulfillConstraint
        (Beam.FromBackendRow backend)
        TaskTable
    => Database.TableFieldsFulfillConstraint
        (Beam.HasSqlEqualityCheck backend)
        TaskTable
    => Database.TableFieldsFulfillConstraint
        (BeamSqlBackendCanSerialize backend)
        TaskTable
    => Member (DatabaseL backend) r
    => Sem (TaskL ': r) a
    -> Sem r a
run = interpret \case
  FindByProjectId projectId ->
    (do
      task <- Database.all #tasks
      guard_ (task ^. #projectId ==. val_ (Database.Project.PrimaryKey (WrappedC projectId)))
      pure task)
      |> select
      |> Database.runSelectReturningList
      |> (fmap . fmap) toReadableTask
  Create creatable projectId ->
    insertExpressions [fromCreatableTask creatable projectId]
      |> Database.insert #tasks
      |> Database.runInsertReturningOne
      -- Only @Just@ is acceptable.
      |> fmap Unsafe.fromJust
      |> fmap toReadableTask



toReadableTask :: Database.Task.Row -> Task.Readable
toReadableTask Database.Task.Row {..} =
  Task.Readable { id = coerce id, title, deadlineAt = FormattedOffsetDatetime deadlineAt, projectId = coerce projectId }

fromCreatableTask :: BeamSqlBackend backend
                     => Database.TableFieldsFulfillConstraint
                          (BeamSqlBackendCanSerialize backend)
                          TaskTable
                     => Task.Creatable
                     -> Project.Id
                     -> TaskTable (Beam.QExpr backend s)
fromCreatableTask Task.Creatable {..} projectId =
  Database.Task.Row { id = default_, title = val_ title, deadlineAt = val_ $ unFormattedOffsetDatetime deadlineAt, projectId = val_ $ Database.Project.PrimaryKey (WrappedC projectId)   }
