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
import qualified Cascade.Api.Data.Session      as Session
import           Cascade.Api.Data.Session       ( Session )
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

handleGetById :: Member ProjectL r
              => Project.Id
              -> Session
              -> Sem r (Union GetByIdResponse)
handleGetById id = Session.withAuthenticated \_ ->
  Database.Project.findById id
    >>= maybe (respond Response.notFound) (respond . Response.ok)

handleUpdateById :: Member ProjectL r
                 => Project.Id
                 -> Project.Updatable
                 -> Session
                 -> Sem r (Union UpdateByIdResponse)
handleUpdateById id updatable = Session.withAuthenticated \_ ->
  Database.Project.updateById id updatable
    >>= maybe (respond Response.notFound) (respond . Response.ok)

handleDeleteById :: Member ProjectL r
                 => Project.Id
                 -> Session
                 -> Sem r (Union DeleteByIdResponse)
handleDeleteById id = Session.withAuthenticated \_ ->
  Database.Project.deleteById id
    >>= maybe (respond Response.notFound) (respond . Response.ok)

server :: Member ProjectL r => ToServant Routes (AsServerT (Sem r))
server = genericServerT Routes { getById    = handleGetById
                               , updateById = handleUpdateById
                               , deleteById = handleDeleteById
                               }
