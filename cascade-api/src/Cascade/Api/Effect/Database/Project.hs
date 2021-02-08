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
  , findAllByUserId
  , findById
  , create
  , updateById
  , deleteById
  , run
  ) where

import qualified Cascade.Api.Data.Project      as Project
import qualified Cascade.Api.Data.User         as User
import           Cascade.Api.Data.WrappedC
import           Cascade.Api.Database.ProjectTable
                                                ( ProjectTable )
import qualified Cascade.Api.Database.ProjectTable
                                               as ProjectTable
import qualified Cascade.Api.Effect.Database   as Database
import           Cascade.Api.Effect.Database    ( DatabaseL )
import qualified Cascade.Api.Query             as Query
import qualified Cascade.Api.Query.Project     as Project
import qualified Cascade.Api.Query.User        as User
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
  FindAll         ::ProjectL m [Project.Readable]
  FindAllByUserId ::User.Id -> ProjectL m [Project.Readable]
  FindById        ::Project.Id -> ProjectL m (Maybe Project.Readable)
  Create          ::Project.Creatable -> User.Id -> ProjectL m Project.Readable
  UpdateById      ::Project.Id -> Project.Updatable -> ProjectL m (Maybe Project.Readable)
  DeleteById      ::Project.Id -> ProjectL m (Maybe Project.Readable)

makeSem ''ProjectL

run :: forall backend r a
     . BeamSqlBackend backend
    => Beam.HasQBuilder backend
    => Query.TableFieldsFulfillConstraints
         '[ Beam.FromBackendRow backend
          , Beam.HasSqlEqualityCheck backend
          , BeamSqlBackendCanSerialize backend
          ]
         ProjectTable
    => Member (DatabaseL backend) r => Sem (ProjectL ': r) a -> Sem r a
run = interpret \case
  FindAll ->
    Project.queryAll
      |> select
      |> Database.runSelectReturningList
      |> (fmap . fmap) toReadableProject
  FindAllByUserId userId ->
    User.queryAllRelatedProjectsById userId
      |> select
      |> Database.runSelectReturningList
      |> (fmap . fmap) toReadableProject
  FindById id ->
    Query.lookup #projects (ProjectTable.PrimaryKey $ coerce id)
      |> Database.runSelectReturningOne
      |> (fmap . fmap) toReadableProject
  Create creatable userId ->
    insertExpressions [fromCreatableProject creatable]
      |> Query.insert #projects
      |> Database.runInsertReturningOne
      -- Only @Just@ is acceptable.
      |> fmap Unsafe.fromJust
      |> fmap toReadableProject
  UpdateById id updatable -> case updatable of
    Project.Updatable { name = Nothing } -> run $ findById id
    _ ->
      Query.update #projects
                   (fromUpdatableProject updatable)
                   (\project -> project ^. #id ==. val_ (coerce id))
        |> Database.runUpdateReturningOne
        |> (fmap . fmap) toReadableProject
  DeleteById id ->
    Query.delete #projects (\project -> project ^. #id ==. val_ (coerce id))
      |> Database.runDeleteReturningOne
      |> (fmap . fmap) toReadableProject

toReadableProject :: ProjectTable.Row -> Project.Readable
toReadableProject ProjectTable.Row {..} =
  Project.Readable { id = coerce id, name }

fromCreatableProject :: BeamSqlBackend backend
                     => Query.TableFieldsFulfillConstraint
                          (BeamSqlBackendCanSerialize backend)
                          ProjectTable
                     => Project.Creatable
                     -> ProjectTable (Beam.QExpr backend s)
fromCreatableProject Project.Creatable {..} =
  ProjectTable.Row { id = default_, name = val_ name }

fromUpdatableProject :: BeamSqlBackend backend
                     => Query.TableFieldsFulfillConstraint
                          (BeamSqlBackendCanSerialize backend)
                          ProjectTable
                     => Project.Updatable
                     -> (  forall s
                         . ProjectTable (Beam.QField s)
                        -> Beam.QAssignment backend s
                        )
fromUpdatableProject updatable project =
  mconcat . catMaybes $ [(project ^. #name <-.) . val_ <$> updatable ^. #name]
