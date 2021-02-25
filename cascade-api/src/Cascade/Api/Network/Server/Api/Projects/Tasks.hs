{-|
Module      : Cascade.Api.Network.Server.Api.Projects.Tasks
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.Server.Api.Projects.Tasks
  ( server
  )
where

import qualified Cascade.Api.Data.Task         as Task
import qualified Cascade.Api.Data.Project      as Project
import qualified Cascade.Api.Effect.Database.Task
                                               as Database.Task
import qualified Cascade.Api.Effect.Database.Project
                                               as Database.Project
import           Cascade.Api.Effect.Database.Task
                                                ( TaskL )
import           Cascade.Api.Effect.Database.Project
                                                ( ProjectL )
import qualified Cascade.Api.Effect.Time       as Time
import           Cascade.Api.Effect.Time        ( TimeL )
import           Cascade.Api.Network.Anatomy.Api.Projects.Tasks
import qualified Cascade.Api.Servant.Response  as Response
import           Polysemy                       ( Member
                                                , Members
                                                , Sem
                                                )

import           Polysemy.Error                 ( Error
                                                , throw
                                                )
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic         ( AsServerT
                                                , genericServerT
                                                )
import           Validation                     ( validation )

handleCreate
  :: Members '[TaskL, ProjectL, TimeL] r
  => Project.Id
  -> Task.RawCreatable
  -> Sem r (Union CreateResponse)
handleCreate projectId creatable =
  Time.now
    >>= validation (respond . Response.Unprocessable) go
    .   Task.parseRawCreatableTask creatable
 where
  go
    :: Members '[TaskL, ProjectL, TimeL] r
    => Task.ParsedCreatable
    -> Sem r (Union CreateResponse)
  go parsedCreatable = Database.Project.findById projectId >>= maybe
    (respond Response.notFound)
    \_ ->
      Database.Task.create parsedCreatable projectId
        >>= respond
        .   Response.created

handleGetByProjectId
  :: Member TaskL r => Project.Id -> Sem r (Union GetByProjectIdResponse)
handleGetByProjectId projectId =
  Database.Task.findByProjectId projectId >>= respond . Response.ok

server
  :: Members '[TaskL, ProjectL, TimeL] r
  => Project.Id
  -> ToServant Routes (AsServerT (Sem r))
server projectId = genericServerT Routes
  { getByProjectId = handleGetByProjectId projectId
  , create         = handleCreate projectId
  }
