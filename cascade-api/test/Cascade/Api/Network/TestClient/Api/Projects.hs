{-|
Module      : Cascade.Api.Network.TestClient.Api.Projects
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.TestClient.Api.Projects
  ( CreateResponse
  , GetAllResponse
  , GetByIdResponse
  , UpdateByIdResponse
  , DeleteByIdResponse
  , create
  , getAll
  , getById
  , updateById
  , deleteById
  , tasks
  ) where

import qualified Cascade.Api.Data.Project           as Project
import qualified Cascade.Api.Network.Anatomy.Api.Projects
                                                    as Api.Projects

import qualified Cascade.Api.Network.Anatomy.Api.Projects.Tasks
                                                    as Api.Projects.Tasks
import           Cascade.Api.Network.TestClient      ( interpret )
import qualified Cascade.Api.Network.TestClient.Api as Client.Api
import           Control.Lens                        ( (^.) )
import           Control.Monad.Free                  ( Free )
import           Data.Generics.Labels                ( )
import           Prelude                      hiding ( getAll )
import           Servant.API                         ( Union )
import           Servant.API.Generic                 ( fromServant )
import           Servant.Client.Free                 ( ResponseF )
import           Servant.Client.Free                 ( ClientF )
import           Servant.Client.Generic              ( AsClientT )

type CreateResponse = (ResponseF (Union Api.Projects.CreateResponse))

type GetAllResponse = (ResponseF (Union Api.Projects.GetAllResponse))

type GetByIdResponse = (ResponseF (Union Api.Projects.GetByIdResponse))

type UpdateByIdResponse = (ResponseF (Union Api.Projects.UpdateByIdResponse))

type DeleteByIdResponse = (ResponseF (Union Api.Projects.DeleteByIdResponse))

create :: Project.Creatable -> IO CreateResponse
create = interpret . go where go = Client.Api.projects ^. #create

getAll :: IO GetAllResponse
getAll = interpret go where go = Client.Api.projects ^. #getAll

getById :: Project.Id -> IO GetByIdResponse
getById = interpret . go where go = Client.Api.projects ^. #getById

updateById :: Project.Id -> Project.Updatable -> IO UpdateByIdResponse
updateById id updatable = interpret $ go id updatable where go = Client.Api.projects ^. #updateById

deleteById :: Project.Id -> IO DeleteByIdResponse
deleteById = interpret . go where go = Client.Api.projects ^. #deleteById

tasks :: Project.Id -> Api.Projects.Tasks.Routes (AsClientT (Free ClientF))
tasks projectId = fromServant (Client.Api.projects ^. #tasks $ projectId)
