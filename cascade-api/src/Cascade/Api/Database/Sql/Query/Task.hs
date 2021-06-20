{-|
Module      : Cascade.Api.Database.Sql.Query.Task
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

module Cascade.Api.Database.Sql.Query.Task
    ( byId
    , byProjectId
    ) where

import qualified Cascade.Api.Data.Project          as Project
import qualified Cascade.Api.Data.Task             as Task
import           Cascade.Api.Data.WrappedC         ( WrappedC (..) )
import qualified Cascade.Api.Database.ProjectTable as ProjectTable
import           Cascade.Api.Database.Sql          ( Q )
import qualified Cascade.Api.Database.Sql          as SQL
import qualified Cascade.Api.Database.Sql.Query    as SQL.Query
import           Cascade.Api.Database.TaskTable    ( TaskTable )
import qualified Database.Beam                     as Beam

byId :: _ => Task.Id -> Q backend s (TaskTable (Beam.QExpr backend s))
byId id = SQL.Query.all #tasks |> SQL.filter (#id `SQL.eq` SQL.literal id)

byProjectId :: _ => Project.Id -> Q backend s (TaskTable (Beam.QExpr backend s))
byProjectId projectId = SQL.Query.all #tasks |> SQL.filter (#projectId `SQL.eq` Beam.val_ (coerce projectId))
