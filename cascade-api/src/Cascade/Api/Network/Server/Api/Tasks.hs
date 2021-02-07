module Cascade.Api.Network.Server.Api.Tasks
  ( server
  ) where

import qualified Cascade.Api.Data.Task         as Task
import qualified Cascade.Api.Data.Project      as Project
import qualified Cascade.Api.Effect.Database.Task
                                               as Database.Task
import           Cascade.Api.Effect.Database.Task
                                                ( TaskL )
import           Cascade.Api.Network.Anatomy.Api.Tasks
import qualified Cascade.Api.Servant.Response  as Response
import           Polysemy                       ( Member
                                                , Sem
                                                )
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic         ( AsServerT
                                                , genericServerT
                                                )

handleCreate :: Member TaskL r
             => Project.Id
             -> Task.Creatable
             -> Sem r (Union CreateResponse)
handleCreate projectId creatable =
  Database.Task.create creatable projectId >>= respond . Response.created

handleFindByProjectId :: Member TaskL r => Project.Id -> Sem r (Union FindByProjectIdResponse)
handleFindByProjectId projectId = Database.Task.findByProjectId projectId >>= respond . Response.ok

server :: Member TaskL r => ToServant Routes (AsServerT (Sem r))
server = genericServerT Routes { findByProjectId = handleFindByProjectId
                               , create          = handleCreate
                               }
