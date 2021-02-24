{-|
Module      : Cascade.Api.Network.Server.Api.User.Projects
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.Server.Api.User.Projects
  ( server
  ) where

import qualified Cascade.Api.Data.Project      as Project
import           Cascade.Api.Data.Session       ( Session )
import qualified Cascade.Api.Data.Session      as Session
import qualified Cascade.Api.Effect.Depository as Depository
import qualified Cascade.Api.Effect.Depository.Project
                                               as Depository.Project
import           Cascade.Api.Network.Anatomy.Api.User.Projects
import qualified Cascade.Api.Servant.Response  as Response
import           Control.Lens                   ( (^.) )
import           Polysemy
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic

handleCreate :: Member Depository.ProjectL r
             => Project.Creatable
             -> Session
             -> Sem r (Union CreateResponse)
handleCreate creatable = Session.withAuthenticated \claims ->
  Depository.Project.create creatable (claims ^. #userId)
    >>= respond
    .   Response.created

handleGetAll :: Member Depository.ProjectL r
             => Session
             -> Sem r (Union GetAllResponse)
handleGetAll = Session.withAuthenticated \claims ->
  Depository.Project.findAllByUserId (claims ^. #userId)
    >>= respond
    .   Response.ok

server :: Member Depository.ProjectL r => ToServant Routes (AsServerT (Sem r))
server = genericServerT Routes { create = handleCreate, getAll = handleGetAll }
