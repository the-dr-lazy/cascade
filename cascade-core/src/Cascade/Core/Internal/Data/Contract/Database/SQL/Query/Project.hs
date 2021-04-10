{-|
Module      : Cascade.Core.Internal.Data.Contract.Database.SQL.Query.Project
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

module Cascade.Core.Internal.Data.Contract.Database.SQL.Query.Project (byId, byUser, byUserId) where

import qualified Cascade.Core.Data.Model.Project    as Project
import qualified Cascade.Core.Data.Model.User       as User
import           Cascade.Core.Internal.Data.Contract.Database.ProjectTable
                                                    as ProjectTable
import           Cascade.Core.Internal.Data.Contract.Database.SQL
                                                     ( Q
                                                     , filterProjectsByRelatedUsers
                                                     )
import qualified Cascade.Core.Internal.Data.Contract.Database.SQL
                                                    as SQL
import qualified Cascade.Core.Internal.Data.Contract.Database.SQL.Query
                                                    as SQL.Query
import qualified Cascade.Core.Internal.Data.Contract.Database.SQL.Query.User
                                                    as SQL.Query.User
import           Cascade.Core.Internal.Data.Contract.Database.UserTable
                                                     ( UserTable )
import qualified Cascade.Core.Internal.Data.Model.Project.Id
                                                    as Project.Id
import qualified Database.Beam                      as Beam

byId :: _ => Project.Id p -> Q backend s (ProjectTable (Beam.QExpr backend s))
byId (Project.Id.un -> id) = SQL.Query.all #projects |> SQL.filter (#id `SQL.eq` SQL.literal id)

byUser :: _ => Q backend s (UserTable (Beam.QExpr backend s)) -> Q backend s (ProjectTable (Beam.QExpr backend s))
byUser q = SQL.Query.all #projects |> filterProjectsByRelatedUsers q

byUserId :: _ => User.Id p -> Q backend s (ProjectTable (Beam.QExpr backend s))
byUserId = byUser . SQL.Query.User.byId
