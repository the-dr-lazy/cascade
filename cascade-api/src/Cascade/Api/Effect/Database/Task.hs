{-|
Module      : Cascade.Api.Effect.Database.Task
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Effect.Database.Task (TaskL(..), findByProjectId, findById, create, updateById, deleteById, run) where

import           Cascade.Api.Data.OffsetDatetime     ( FormattedOffsetDatetime(..) )
import qualified Cascade.Api.Data.OffsetDatetime.Deadline
                                                    as Deadline
import qualified Cascade.Api.Data.Project           as Project
import qualified Cascade.Api.Data.Task              as Task
import qualified Cascade.Api.Data.Text.Title        as Title
import           Cascade.Api.Data.WrappedC
import qualified Cascade.Api.Database.ProjectTable  as ProjectTable
import qualified Cascade.Api.Database.Sql           as SQL
import qualified Cascade.Api.Database.Sql.Query.Task
                                                    as SQL.Query.Task
import           Cascade.Api.Database.TaskTable      ( TaskTable )
import qualified Cascade.Api.Database.TaskTable     as TaskTable
import qualified Cascade.Api.Effect.Database        as Database
import           Cascade.Api.Effect.Database         ( DatabaseL )
import qualified Cascade.Data.Validation            as Validation
import           Control.Lens                        ( (^.)
                                                     , to
                                                     )
import           Database.Beam                       ( (<-.)
                                                     , insertExpressions
                                                     , val_
                                                     )
import qualified Database.Beam                      as Beam
import           Database.Beam.Backend               ( BeamSqlBackend
                                                     , BeamSqlBackendCanSerialize
                                                     )
import           Polysemy                            ( Member
                                                     , Sem
                                                     , interpret
                                                     , makeSem
                                                     )
import qualified Relude.Unsafe                      as Unsafe
                                                     ( fromJust )

data TaskL m a where
  FindByProjectId ::Project.Id -> TaskL m [Task.Readable]
  FindById        ::Task.Id -> TaskL m (Maybe Task.Readable)
  Create          ::Task.Creatable 'Validation.Parsed -> Project.Id -> TaskL m Task.Readable
  UpdateById      ::Task.Id -> Task.Updatable 'Validation.Parsed -> TaskL m (Maybe Task.Readable)
  DeleteById      ::Task.Id -> TaskL m (Maybe Task.Readable)

makeSem ''TaskL

run :: forall backend r a
     . BeamSqlBackend backend
    => Beam.HasQBuilder backend
    => SQL.TableFieldsFulfillConstraints
         '[Beam.FromBackendRow backend , Beam.HasSqlEqualityCheck backend , BeamSqlBackendCanSerialize backend]
         TaskTable
    => Member (DatabaseL backend) r => Sem (TaskL ': r) a -> Sem r a
run = interpret \case
  FindByProjectId projectId ->
    SQL.Query.Task.byProjectId projectId |> SQL.select |> Database.runSelectReturningList |> (fmap . fmap) toReadableTask
  FindById id -> SQL.Query.Task.byId id |> SQL.select |> Database.runSelectReturningOne |> (fmap . fmap) toReadableTask
  Create creatable projectId ->
    insertExpressions [fromParsedCreatableTask projectId creatable]
      |> SQL.insert #tasks
      |> Database.runInsertReturningOne
      -- Only @Just@ is acceptable.
      |> fmap Unsafe.fromJust
      |> fmap toReadableTask
  UpdateById id updatable -> case updatable of
    Task.Updatable { title = Nothing, deadlineAt = Nothing } -> run $ findById id
    _ ->
      SQL.update #tasks (fromParsedUpdatableTask updatable) (#id `SQL.eq` SQL.literal id)
        |> Database.runUpdateReturningOne
        |> (fmap . fmap) toReadableTask
  DeleteById id -> SQL.delete #tasks (#id `SQL.eq` SQL.literal id) |> Database.runDeleteReturningOne |> (fmap . fmap) toReadableTask

toReadableTask :: TaskTable.Row -> Task.Readable
toReadableTask TaskTable.Row {..} =
  Task.Readable { id = coerce id, title = coerce title, deadlineAt = coerce deadlineAt, projectId = coerce projectId }

fromParsedCreatableTask :: BeamSqlBackend backend
                        => SQL.TableFieldsFulfillConstraint (BeamSqlBackendCanSerialize backend) TaskTable
                        => Project.Id
                        -> Task.Creatable 'Validation.Parsed
                        -> TaskTable (Beam.QExpr backend s)
fromParsedCreatableTask projectId creatable = TaskTable.Row { id         = SQL.def
                                                            , title      = creatable ^. #title . to coerce . to val_
                                                            , deadlineAt = creatable ^. #deadlineAt . to Deadline.un . to val_
                                                            , projectId  = projectId |> coerce |> val_
                                                            }

fromParsedUpdatableTask :: BeamSqlBackend backend
                        => SQL.TableFieldsFulfillConstraint (BeamSqlBackendCanSerialize backend) TaskTable
                        => Task.Updatable 'Validation.Parsed
                        -> (forall s . TaskTable (Beam.QField s) -> Beam.QAssignment backend s)
fromParsedUpdatableTask updatable task =
  mconcat
    . catMaybes
    $ [ (task ^. #title <-.) . (val_ . coerce) <$> updatable ^. #title
      , (task ^. #deadlineAt <-.) . (val_ . Deadline.un) <$> (updatable ^. #deadlineAt)
      ]
