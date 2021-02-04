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

handleGetAll :: Member TaskL r => Project.Id -> Sem r (Union GetAllResponse)
handleGetAll projectId = Database.Task.findByProjectId projectId >>= respond . Response.ok

-- handleGetById :: Member ProjectL r
--               => Project.Id
--               -> Sem r (Union GetByIdResponse)
-- handleGetById id = Database.Project.findById id
--   >>= maybe (respond Response.notFound) (respond . Response.ok)

-- handleUpdateById :: Member ProjectL r
--                  => Project.Id
--                  -> Project.Updatable
--                  -> Sem r (Union UpdateByIdResponse)
-- handleUpdateById id updatable =
--   Database.Project.updateById id updatable
--     >>= maybe (respond Response.notFound) (respond . Response.ok)

-- handleDeleteById :: Member ProjectL r
--                  => Project.Id
--                  -> Sem r (Union DeleteByIdResponse)
-- handleDeleteById id = Database.Project.deleteById id
--   >>= maybe (respond Response.notFound) (respond . Response.ok)

server :: Member TaskL r => ToServant Routes (AsServerT (Sem r))
server = genericServerT Routes { getAll     = handleGetAll
                               -- , getById    = handleGetById
                               , create     = handleCreate
                               -- , updateById = handleUpdateById
                               -- , deleteById = handleDeleteById
                               }
