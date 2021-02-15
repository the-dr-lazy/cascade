module Cascade.Api.Network.TestClient.Api.Projects.Tasks
  ( CreateResponse
  , create
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

create :: Project.Id -> Task.RawCreatable -> IO CreateResponse
create projectId = interpret . go
  where go = Client.Api.Projects.tasks projectId ^. #create
