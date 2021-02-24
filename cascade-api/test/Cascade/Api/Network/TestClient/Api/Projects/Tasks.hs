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

module Cascade.Api.Network.TestClient.Api.Projects.Tasks
  ( CreateResponse
  , FindByProjectIdResponse
  , create
  , findByProjectId
  )
where

import qualified Cascade.Api.Data.Task         as Task
import qualified Cascade.Api.Data.Project      as Project
import qualified Cascade.Api.Network.Anatomy.Api.Projects.Tasks
                                               as Api.Projects.Tasks
import           Cascade.Api.Network.TestClient ( interpret )
import qualified Cascade.Api.Network.TestClient.Api.Projects
                                               as Client.Api.Projects
import           Control.Lens                   ( (^.) )
import           Data.Generics.Labels           ( )
import           Servant.API                    ( Union )
import           Servant.Client.Free            ( ResponseF )

type CreateResponse = ResponseF (Union Api.Projects.Tasks.CreateResponse)

type FindByProjectIdResponse
  = ResponseF (Union Api.Projects.Tasks.FindByProjectIdResponse)

create :: Project.Id -> Task.RawCreatable -> IO CreateResponse
create projectId = interpret . go
  where go = Client.Api.Projects.tasks projectId ^. #create

findByProjectId :: Project.Id -> IO FindByProjectIdResponse
findByProjectId projectId = interpret go
  where go = Client.Api.Projects.tasks projectId ^. #findByProjectId
