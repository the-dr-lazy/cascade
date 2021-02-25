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

module Cascade.Api.Effect.Database.Task
  ( TaskL(..)
  , findByProjectId
  , findById
  , create
  , updateById
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
                                                , filter_
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
import qualified Cascade.Api.Data.OffsetDatetime.Deadline
                                               as Deadline



data TaskL m a where
  FindByProjectId ::Project.Id -> TaskL m [Task.Readable]
  FindById        ::Task.Id -> TaskL m (Maybe Task.Readable)
  Create          ::Task.ParsedCreatable -> Project.Id -> TaskL m Task.Readable
  UpdateById      ::Task.Id -> Task.ParsedUpdatable -> TaskL m (Maybe Task.Readable)
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
    insertExpressions [fromParsedCreatableTask projectId creatable]
      |> Database.insert #tasks
      |> Database.runInsertReturningOne
      -- Only @Just@ is acceptable.
      |> fmap Unsafe.fromJust
      |> fmap toReadableTask
  UpdateById id updatable -> case updatable of
    Task.ParsedUpdatable { title = Nothing, deadlineAt = Nothing } ->
      run $ findById id
    _ ->
      Database.update #tasks
                      (fromParsedUpdatableTask updatable)
                      (\task -> task ^. #id ==. val_ (coerce id))
        |> Database.runUpdateReturningOne
        |> (fmap . fmap) toReadableTask

  DeleteById id ->
    Database.delete #tasks (\task -> task ^. #id ==. val_ (coerce id))
      |> Database.runDeleteReturningOne
      |> (fmap . fmap) toReadableTask

toReadableTask :: Database.Task.Row -> Task.Readable
toReadableTask Database.Task.Row {..} = Task.Readable
  { id         = coerce id
  , title      = coerce title
  , deadlineAt = coerce deadlineAt
  , projectId  = coerce projectId
  }

fromParsedCreatableTask
  :: BeamSqlBackend backend
  => Database.TableFieldsFulfillConstraint
       (BeamSqlBackendCanSerialize backend)
       TaskTable
  => Project.Id
  -> Task.ParsedCreatable
  -> TaskTable (Beam.QExpr backend s)
fromParsedCreatableTask projectId creatable = Database.Task.Row
  { id         = default_
  , title      = creatable ^. #title . to coerce . to val_
  , deadlineAt = creatable ^. #deadlineAt . to Deadline.un . to val_
  , projectId  = projectId |> coerce |> val_
  }

fromParsedUpdatableTask
  :: BeamSqlBackend backend
  => Database.TableFieldsFulfillConstraint
       (BeamSqlBackendCanSerialize backend)
       TaskTable
  => Task.ParsedUpdatable
  -> (  forall s
      . TaskTable (Beam.QField s)
     -> Beam.QAssignment backend s
     )
fromParsedUpdatableTask updatable task =
  mconcat
    . catMaybes
    $ [ (task ^. #title <-.) . (val_ . coerce) <$> updatable ^. #title
      , (task ^. #deadlineAt <-.)
      .   (val_ . Deadline.un)
      <$> (updatable ^. #deadlineAt)
      ]
