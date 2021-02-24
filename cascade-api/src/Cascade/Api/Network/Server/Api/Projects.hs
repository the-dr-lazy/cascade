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

import qualified Cascade.Api.Data.Project      as Project
import qualified Cascade.Api.Effect.Depository as Depository
import qualified Cascade.Api.Effect.Depository.Project
                                               as Depository.Project
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

handleGetById :: Member Depository.ProjectL r
              => Project.Id
              -> Sem r (Union GetByIdResponse)
handleGetById id = Depository.Project.findById id
  >>= maybe (respond Response.notFound) (respond . Response.ok)

handleUpdateById :: Member Depository.ProjectL r
                 => Project.Id
                 -> Project.Updatable
                 -> Sem r (Union UpdateByIdResponse)
handleUpdateById id updatable =
  Depository.Project.updateById id updatable
    >>= maybe (respond Response.notFound) (respond . Response.ok)

handleDeleteById :: Member Depository.ProjectL r
                 => Project.Id
                 -> Sem r (Union DeleteByIdResponse)
handleDeleteById id = Depository.Project.deleteById id
  >>= maybe (respond Response.notFound) (respond . Response.ok)

server :: Member Depository.ProjectL r => ToServant Routes (AsServerT (Sem r))
server = genericServerT Routes { getById    = handleGetById
                               , updateById = handleUpdateById
                               , deleteById = handleDeleteById
                               }
