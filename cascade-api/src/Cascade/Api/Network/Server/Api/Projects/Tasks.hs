module Cascade.Api.Network.Server.Api.Projects.Tasks
  ( server
  )
where

import qualified Cascade.Api.Data.Task         as Task
import qualified Cascade.Api.Data.Project      as Project
import qualified Cascade.Api.Effect.Database.Task
                                               as Database.Task
import           Cascade.Api.Effect.Database.Task
                                                ( TaskL )
import           Cascade.Api.Effect.Time        ( TimeL
                                                , now
                                                )
import           Cascade.Api.Network.Anatomy.Api.Projects.Tasks
import qualified Cascade.Api.Servant.Response  as Response
import           Polysemy                       ( Member
                                                , Members
                                                , Sem
                                                )
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic         ( AsServerT
                                                , genericServerT
                                                )
import           Validation                     ( validation )

handleCreate
  :: Members '[TaskL, TimeL] r
  => Project.Id
  -> Task.RawCreatable
  -> Sem r (Union CreateResponse)
handleCreate projectId creatable =
  now
    >>= validation (respond . Response.Unprocessable) go
    .   Task.parseRawCreatableTask creatable
 where
  go
    :: Members '[TaskL, TimeL] r
    => Task.ParsedCreatable
    -> Sem r (Union CreateResponse)
  go creatable =
    Database.Task.create creatable projectId >>= respond . Response.created

handleFindByProjectId
  :: Member TaskL r => Project.Id -> Sem r (Union FindByProjectIdResponse)
handleFindByProjectId projectId =
  Database.Task.findByProjectId projectId >>= respond . Response.ok

server :: Members '[TaskL, TimeL] r => ToServant Routes (AsServerT (Sem r))
server = genericServerT Routes { findByProjectId = handleFindByProjectId
                               , create          = handleCreate
                               }
