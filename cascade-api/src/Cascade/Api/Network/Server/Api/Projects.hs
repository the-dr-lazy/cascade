{-|
Module      : Cascade.Api.Network.Server.Api.Projects
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.Server.Api.Projects
  ( server
  ) where

import           Cascade.Api.Data.Project
import qualified Cascade.Api.Data.Project      as Project
import qualified Cascade.Api.Effect.Database.Project
                                               as Database.Project
import           Cascade.Api.Effect.Database.Project
                                                ( ProjectL )
import           Cascade.Api.Network.Anatomy.Api.Projects
import qualified Cascade.Api.Servant.Response  as Response
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
             -> Sem r (Union CreateResponse)
handleCreate creatable =
  Database.Project.create creatable >>= respond . Response.created

handleGetAll :: Member ProjectL r => Sem r (Union GetAllResponse)
handleGetAll = Database.Project.findAll >>= respond . Response.ok

handleGetById :: Member ProjectL r
              => Project.Id
              -> Sem r (Union GetByIdResponse)
handleGetById id = Database.Project.findById id
  >>= maybe (respond Response.notFound) (respond . Response.ok)

handleUpdateById :: Member ProjectL r
                 => Project.Id
                 -> Updatable Project
                 -> Sem r (Union UpdateByIdResponse)
handleUpdateById id updatable =
  Database.Project.updateById id updatable
    >>= maybe (respond Response.notFound) (respond . Response.ok)

handleDeleteById :: Member ProjectL r
                 => Project.Id
                 -> Sem r (Union DeleteByIdResponse)
handleDeleteById id = Database.Project.deleteById id
  >>= maybe (respond Response.notFound) (respond . Response.ok)

server :: Member ProjectL r => ToServant Routes (AsServerT (Sem r))
server = genericServerT Routes { getAll     = handleGetAll
                               , getById    = handleGetById
                               , create     = handleCreate
                               , updateById = handleUpdateById
                               , deleteById = handleDeleteById
                               }
