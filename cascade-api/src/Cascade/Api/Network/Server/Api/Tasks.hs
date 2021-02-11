module Cascade.Api.Network.Server.Api.Tasks
  ( server
  )
where

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

handleGetById :: Member TaskL r => Task.Id -> Sem r (Union GetByIdResponse)
handleGetById id = Database.Task.findById id
  >>= maybe (respond Response.notFound) (respond . Response.ok)

handleDeleteById
  :: Member TaskL r => Task.Id -> Sem r (Union DeleteByIdResponse)
handleDeleteById id = Database.Task.deleteById id
  >>= maybe (respond Response.notFound) (respond . Response.ok)

server :: Member TaskL r => ToServant Routes (AsServerT (Sem r))
server = genericServerT Routes { getById    = handleGetById
                               , deleteById = handleDeleteById
                               }
