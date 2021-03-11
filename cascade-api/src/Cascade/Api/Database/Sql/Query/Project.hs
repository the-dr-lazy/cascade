{-|
Module      : Cascade.Api.Database.Sql.Query.Project
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Cascade.Api.Database.Sql.Query.Project
  ( byId
  , byUser
  , byUserId
  ) where

import qualified Cascade.Api.Data.Project      as Project
import qualified Cascade.Api.Data.User         as User
import           Cascade.Api.Data.WrappedC      ( WrappedC(..) )
import           Cascade.Api.Database.ProjectTable
                                               as ProjectTable
import           Cascade.Api.Database.Sql       ( Q
                                                , filterProjectsByRelatedUsers
                                                )
import qualified Cascade.Api.Database.Sql      as SQL
import qualified Cascade.Api.Database.Sql.Query
                                               as SQL.Query
import qualified Cascade.Api.Database.Sql.Query.User
                                               as SQL.Query.User
import           Cascade.Api.Database.UserTable ( UserTable )
import qualified Database.Beam                 as Beam

byId :: _ => Project.Id -> Q backend s (ProjectTable (Beam.QExpr backend s))
byId id = SQL.Query.all #projects |> SQL.filter (#id `SQL.eq` SQL.literal id)

byUser :: _
       => Q backend s (UserTable (Beam.QExpr backend s))
       -> Q backend s (ProjectTable (Beam.QExpr backend s))
byUser q = SQL.Query.all #projects |> filterProjectsByRelatedUsers q

byUserId :: _ => User.Id -> Q backend s (ProjectTable (Beam.QExpr backend s))
byUserId = byUser . SQL.Query.User.byId
