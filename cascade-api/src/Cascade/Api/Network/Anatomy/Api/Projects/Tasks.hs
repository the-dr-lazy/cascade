module Cascade.Api.Network.Anatomy.Api.Projects.Tasks
  ( Routes(..)
  , CreateResponse
  , FindByProjectIdResponse
  )
where

import qualified Cascade.Api.Data.Project      as Project
import qualified Cascade.Api.Data.Task         as Task
import           Cascade.Api.Network.Anatomy.Prelude
import qualified Cascade.Api.Servant.Response  as Response
import           Data.Generics.Labels           ( )

type CreateResponse = '[Response.Created Task.Readable]

type FindByProjectIdResponse = '[Response.Ok [Task.Readable]]

data Routes route = Routes
  { create :: route :- Capture "id" Project.Id :> "tasks" :> ReqBody '[JSON] Task.Creatable :> Post '[JSON] CreateResponse
  , findByProjectId :: route :- Capture "id" Project.Id :> "tasks" :> Get '[JSON] FindByProjectIdResponse
  }
  deriving stock Generic
