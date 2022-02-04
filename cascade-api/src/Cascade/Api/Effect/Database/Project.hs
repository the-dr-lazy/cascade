{-|
Module      : Cascade.Api.Effect.Database.Project
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2022 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Effect.Database.Project
    ( ProjectL (..)
    , create
    , deleteById
    , doesExistsById
    , findAll
    , findAllByUserId
    , findById
    , run
    , updateById
    ) where

import qualified Cascade.Api.Data.Project               as Project
import qualified Cascade.Api.Data.User                  as User
import           Cascade.Api.Data.WrappedC
import           Cascade.Api.Database.ProjectTable      ( ProjectTable )
import qualified Cascade.Api.Database.ProjectTable      as ProjectTable
import qualified Cascade.Api.Database.Sql               as SQL
import qualified Cascade.Api.Database.Sql.Query         as SQL.Query
import qualified Cascade.Api.Database.Sql.Query.Project as SQL.Query.Project
import qualified Cascade.Api.Database.UserProjectTable  as UserProjectTable
import qualified Cascade.Api.Database.UserTable         as UserTable
import           Cascade.Api.Effect.Database            ( DatabaseL )
import qualified Cascade.Api.Effect.Database            as Database
import           Control.Lens                           ( (^.) )
import           Database.Beam                          ( insertExpressions, pk, val_, (<-.) )
import qualified Database.Beam                          as Beam
import           Database.Beam.Backend
    ( BeamSqlBackend, BeamSqlBackendCanSerialize )
import           Polysemy                               ( Member, Sem, interpret, makeSem )
import qualified Relude.Unsafe                          as Unsafe ( fromJust )

data ProjectL m a where
  FindAll :: ProjectL m [Project.Readable]
  FindAllByUserId :: User.Id -> ProjectL m [Project.Readable]
  FindById :: Project.Id -> ProjectL m (Maybe Project.Readable)
  Create :: Project.Creatable -> User.Id -> ProjectL m Project.Readable
  UpdateById :: Project.Id -> Project.Updatable -> ProjectL m (Maybe Project.Readable)
  DeleteById :: Project.Id -> ProjectL m (Maybe Project.Readable)
  DoesExistsById :: Project.Id -> ProjectL m Bool

makeSem ''ProjectL

run :: forall backend r a
     . BeamSqlBackend backend
    => Beam.HasQBuilder backend
    => SQL.TableFieldsFulfillConstraints
         '[Beam.FromBackendRow backend , Beam.HasSqlEqualityCheck backend , BeamSqlBackendCanSerialize backend]
         ProjectTable
    => Beam.FromBackendRow backend Bool => Member (DatabaseL backend) r => Sem (ProjectL ': r) a -> Sem r a
run = interpret \case
  FindAll -> SQL.Query.all #projects |> SQL.select |> Database.runSelectReturningList |> (fmap . fmap) toReadableProject
  FindAllByUserId userId ->
    SQL.Query.Project.byUserId userId |> SQL.select |> Database.runSelectReturningList |> (fmap . fmap) toReadableProject
  FindById id             -> SQL.Query.Project.byId id |> SQL.select |> Database.runSelectReturningOne |> (fmap . fmap) toReadableProject
  Create creatable userId -> Database.withTransaction do
    project <-
      insertExpressions [fromCreatableProject creatable]
      |> SQL.insert #projects
      |> Database.runInsertReturningOne
      -- Only @Just@ is acceptable.
      |> fmap Unsafe.fromJust
    insertExpressions
        [ UserProjectTable.Row { userId    = UserTable.PrimaryKey <| SQL.literal userId
                               , projectId = pk <| val_ project
                               , createdAt = SQL.def
                               , updatedAt = SQL.def
                               }
        ]
      |> SQL.insert #userProjects
      |> Database.runInsert
      |> void
    pure <| toReadableProject <| project
  UpdateById id updatable -> case updatable of
    Project.Updatable { name = Nothing } -> run $ findById id
    _ ->
      SQL.update #projects (fromUpdatableProject updatable) (#id `SQL.eq` SQL.literal id)
        |> Database.runUpdateReturningOne
        |> (fmap . fmap) toReadableProject
  DeleteById     id -> SQL.delete #projects (#id `SQL.eq` SQL.literal id) |> Database.runDeleteReturningOne |> (fmap . fmap) toReadableProject
  DoesExistsById id -> do
    let sieve = SQL.filter <| #id `SQL.eq` SQL.literal id

    SQL.Query.existance #projects sieve
      |> SQL.select
      |> Database.runSelectReturningOne
      -- Only @Just@ is acceptable.
      |> fmap Unsafe.fromJust

toReadableProject :: ProjectTable.Row -> Project.Readable
toReadableProject ProjectTable.Row {..} = Project.Readable { id = coerce id, name }

fromCreatableProject :: BeamSqlBackend backend
                     => SQL.TableFieldsFulfillConstraint (BeamSqlBackendCanSerialize backend) ProjectTable
                     => Project.Creatable
                     -> ProjectTable (Beam.QExpr backend s)
fromCreatableProject Project.Creatable {..} = ProjectTable.Row { id = SQL.def, name = SQL.literal name }

fromUpdatableProject :: BeamSqlBackend backend
                     => SQL.TableFieldsFulfillConstraint (BeamSqlBackendCanSerialize backend) ProjectTable
                     => Project.Updatable
                     -> (forall s . ProjectTable (Beam.QField s) -> Beam.QAssignment backend s)
fromUpdatableProject updatable project = mconcat . catMaybes $ [(project ^. #name <-.) . val_ <$> updatable ^. #name]
