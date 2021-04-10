{-|
Module      : Cascade.Core.Internal.Data.Contract.Database.SQL.Query.Task
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

module Cascade.Core.Internal.Data.Contract.Database.SQL.Query.Task (byId, byProjectId) where

import qualified Cascade.Core.Data.Model.Project    as Project
import qualified Cascade.Core.Data.Model.Task       as Task
import qualified Cascade.Core.Internal.Data.Contract.Database.ProjectTable
                                                    as ProjectTable
import           Cascade.Core.Internal.Data.Contract.Database.SQL
                                                     ( Q )
import qualified Cascade.Core.Internal.Data.Contract.Database.SQL
                                                    as SQL
import qualified Cascade.Core.Internal.Data.Contract.Database.SQL.Query
                                                    as SQL.Query
import           Cascade.Core.Internal.Data.Contract.Database.TaskTable
                                                     ( TaskTable )
import qualified Cascade.Core.Internal.Data.Model.Project.Id
                                                    as Project.Id
import qualified Database.Beam                      as Beam

byId :: _ => Task.Id p -> Q backend s (TaskTable (Beam.QExpr backend s))
byId id = SQL.Query.all #tasks |> SQL.filter (#id `SQL.eq` SQL.literal id)

byProjectId :: _ => Project.Id p -> Q backend s (TaskTable (Beam.QExpr backend s))
byProjectId (ProjectTable.PrimaryKey . Project.Id.un -> projectId) = SQL.Query.all #tasks |> SQL.filter (#projectId `SQL.eq` SQL.val projectId)
