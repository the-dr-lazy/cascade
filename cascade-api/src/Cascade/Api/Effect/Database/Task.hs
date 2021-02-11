module Cascade.Api.Effect.Database.Task
  ( TaskL(..)
  , findByProjectId
  , findById
  , create
  , deleteById
  , run
  )
where

import qualified Cascade.Api.Data.Project      as Project
import qualified Cascade.Api.Data.Task         as Task
import           Cascade.Api.Data.OffsetDatetime
                                                ( FormattedOffsetDatetime(..) )
import           Cascade.Api.Data.WrappedC
import           Cascade.Api.Database.Task      ( TaskTable )
import qualified Cascade.Api.Database.Task     as Database.Task
import qualified Cascade.Api.Database.Project  as Database.Project
import qualified Cascade.Api.Effect.Database   as Database
import           Cascade.Api.Effect.Database    ( DatabaseL )
import           Control.Lens                   ( (^.)
                                                , to
                                                )
import           Database.Beam                  ( (<-.)
                                                , (==.)
                                                , default_
                                                , insertExpressions
                                                , select
                                                , val_
                                                , guard_
                                                , filter_
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
  FindByProjectId ::Project.Id -> TaskL m [Task.Readable]
  FindById        ::Task.Id -> TaskL m (Maybe Task.Readable)
  Create          ::Task.ParsedCreatable -> Project.Id -> TaskL m Task.Readable
  DeleteById      ::Task.Id -> TaskL m (Maybe Task.Readable)

makeSem ''TaskL

run
  :: forall backend r a
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
    Database.all #tasks
      |> filter_ (\task -> task ^. #projectId ==. val_ (coerce projectId))
      |> select
      |> Database.runSelectReturningList
      |> (fmap . fmap) toReadableTask
  FindById id ->
    Database.lookup #tasks (Database.Task.PrimaryKey $ coerce id)
      |> Database.runSelectReturningOne
      |> (fmap . fmap) toReadableTask
  Create creatable projectId ->
    insertExpressions [fromCreatableTask projectId creatable]
      |> Database.insert #tasks
      |> Database.runInsertReturningOne
      -- Only @Just@ is acceptable.
      |> fmap Unsafe.fromJust
      |> fmap toReadableTask
  DeleteById id ->
    Database.delete #tasks (\task -> task ^. #id ==. val_ (coerce id))
      |> Database.runDeleteReturningOne
      |> (fmap . fmap) toReadableTask

toReadableTask :: Database.Task.Row -> Task.Readable
toReadableTask Database.Task.Row {..} = Task.Readable
  { id         = coerce id
  , title
  , deadlineAt = coerce deadlineAt
  , projectId  = coerce projectId
  }

fromCreatableTask
  :: BeamSqlBackend backend
  => Database.TableFieldsFulfillConstraint
       (BeamSqlBackendCanSerialize backend)
       TaskTable
  => Project.Id
  -> Task.ParsedCreatable
  -> TaskTable (Beam.QExpr backend s)
fromCreatableTask projectId creatable = Database.Task.Row
  { id         = default_
  , title      = creatable ^. #title . to val_
  , deadlineAt = creatable
                 ^. #deadlineAt
                 .  to unFormattedOffsetDatetime
                 .  to val_
  , projectId  = projectId |> WrappedC |> Database.Project.PrimaryKey |> val_
  }