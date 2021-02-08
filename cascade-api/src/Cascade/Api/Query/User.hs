{-|
Module      : Cascade.Api.Query.User
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Query.User
  ( queryAll
  , queryById
  , queryByUsername
  , queryExistanceByUsernameOrEmailAddress
  , queryAllRelatedProjects
  , queryAllRelatedProjectsById
  ) where

import qualified Cascade.Api.Data.Project      as Project
import qualified Cascade.Api.Data.User         as User
import           Cascade.Api.Data.WrappedC
import           Cascade.Api.Database.ProjectTable
                                                ( ProjectTable )
import           Cascade.Api.Database.UserTable
import           Cascade.Api.Query
import           Control.Lens            hiding ( (|>) )
import           Database.Beam           hiding ( Q )
import           Database.Beam.Backend
import           Prelude                 hiding ( all )

queryAll :: BeamSqlBackend backend => Q backend s (UserTable (QExpr backend s))
queryAll = all #users

type QueryById backend s
  =  BeamSqlBackendCanSerialize backend (WrappedC User.Id)
  => HasSqlEqualityCheck backend (WrappedC User.Id)
  => User.Id
  -> Q backend s (UserTable (QExpr backend s))

queryById :: QueryById backend s
queryById id = queryAll |> filter_ \user -> user ^. #id ==. val_ (coerce id)

type QueryByUsername backend s
  =  BeamSqlBackendCanSerialize backend (WrappedC User.Username)
  => HasSqlEqualityCheck backend (WrappedC User.Username)
  => User.Username
  -> Q backend s (UserTable (QExpr backend s))

queryByUsername :: QueryByUsername backend s
queryByUsername username =
  queryAll |> filter_ \user -> user ^. #username ==. val_ (coerce username)

type QueryExistanceByUsernameOrEmailAddress backend s
  =  HasQBuilder backend
  => HasSqlEqualityCheck backend (WrappedC User.Username)
  => HasSqlEqualityCheck backend (WrappedC User.EmailAddress)
  => BeamSqlBackendCanSerialize backend (WrappedC User.Username)
  => BeamSqlBackendCanSerialize backend (WrappedC User.EmailAddress)
  => User.Username
  -> User.EmailAddress
  -> Q backend s (QExpr backend s Bool)

queryExistanceByUsernameOrEmailAddress :: QueryExistanceByUsernameOrEmailAddress
                                            backend
                                            s
queryExistanceByUsernameOrEmailAddress username emailAddress =
  queryAll |> filter_ prediction |> exists_ |> pure
 where
  prediction user =
    (user ^. #username ==. val_ (coerce username))
      ||. (user ^. #emailAddress ==. val_ (coerce emailAddress))

queryAllRelatedProjects :: HasSqlEqualityCheck backend (WrappedC User.Id)
                        => HasSqlEqualityCheck backend (WrappedC Project.Id)
                        => Q backend s (UserTable (QExpr backend s))
                        -> Q backend s (ProjectTable (QExpr backend s))
queryAllRelatedProjects queryUsers =
  all #projects |> filterProjectsByRelatedUsers queryUsers

type QueryAllRelatedProjectsById backend s
  =  HasSqlEqualityCheck backend (WrappedC User.Id)
  => HasSqlEqualityCheck backend (WrappedC Project.Id)
  => BeamSqlBackendCanSerialize backend (WrappedC User.Id)
  => User.Id
  -> Q backend s (ProjectTable (QExpr backend s))

queryAllRelatedProjectsById :: QueryAllRelatedProjectsById backend s
queryAllRelatedProjectsById = queryAllRelatedProjects . queryById
