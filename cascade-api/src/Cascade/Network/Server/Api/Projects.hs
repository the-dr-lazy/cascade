module Cascade.Network.Server.Api.Projects
  ( server
  ) where

import           Cascade.Data.Api.Project
import qualified Cascade.Data.Api.Project      as Project
import qualified Cascade.Effect.Database.Project
                                               as Database.Project
import           Cascade.Effect.Database.Project
                                                ( ProjectL )
import           Cascade.Network.Anatomy.Api.Projects
import qualified Cascade.Servant.Resource      as Resource
import           Polysemy                       ( Member
                                                , Sem
                                                )
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic         ( AsServerT
                                                , genericServerT
                                                )

handleCreate :: Member ProjectL r
             => Creatable Project
             -> Sem r (Readable Project)
handleCreate = Database.Project.create

handleGetAll :: Member ProjectL r => Sem r [Readable Project]
handleGetAll = Database.Project.findAll

handleGetById :: Member ProjectL r
              => Project.Id
              -> Sem r (Union GetByIdResponse)
handleGetById id = Database.Project.findById id
  >>= maybe (respond Resource.notFound) (respond . Resource.ok)

handleUpdateById :: Member ProjectL r
                 => Project.Id
                 -> Updatable Project
                 -> Sem r (Union UpdateByIdResponse)
handleUpdateById id updatable =
  Database.Project.updateById id updatable
    >>= maybe (respond Resource.notFound) (respond . Resource.ok)

handleDeleteById :: Member ProjectL r
                 => Project.Id
                 -> Sem r (Union DeleteByIdResponse)
handleDeleteById id = Database.Project.deleteById id
  >>= maybe (respond Resource.notFound) (respond . Resource.ok)

server :: Member ProjectL r => ToServant Routes (AsServerT (Sem r))
server = genericServerT Routes { getAll     = handleGetAll
                               , getById    = handleGetById
                               , create     = handleCreate
                               , updateById = handleUpdateById
                               , deleteById = handleDeleteById
                               }
