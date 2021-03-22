{-|
Module      : Cascade.Api.Network.TestClient.Api.Projects.Tasks
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.TestClient.Api.Projects.Tasks (CreateResponse, GetAllByProjectIdResponse, create, getAllByProjectId) where

import qualified Cascade.Api.Data.Project           as Project
import qualified Cascade.Api.Data.Task              as Task
import qualified Cascade.Api.Network.Anatomy.Api.Projects.Tasks
                                                    as Api.Projects.Tasks
import           Cascade.Api.Network.TestClient      ( interpret )
import qualified Cascade.Api.Network.TestClient.Api.Projects
                                                    as Client.Api.Projects
import qualified Cascade.Data.Validation            as Validation
import           Control.Lens                        ( (^.) )
import           Data.Generics.Labels                ( )
import           Servant.API                         ( Union )
import           Servant.Client.Free                 ( ResponseF )

type CreateResponse = ResponseF (Union Api.Projects.Tasks.CreateResponse)

type GetAllByProjectIdResponse = ResponseF (Union Api.Projects.Tasks.GetAllByProjectIdResponse)

create :: Project.Id -> Task.Creatable 'Validation.Raw -> IO CreateResponse
create projectId = interpret . go where go = Client.Api.Projects.tasks projectId ^. #create

getAllByProjectId :: Project.Id -> IO GetAllByProjectIdResponse
getAllByProjectId projectId = interpret go where go = Client.Api.Projects.tasks projectId ^. #getAllByProjectId
