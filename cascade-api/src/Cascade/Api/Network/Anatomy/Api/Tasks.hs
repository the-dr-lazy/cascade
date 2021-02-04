module Cascade.Api.Network.Anatomy.Api.Tasks
  ( Routes(..)
  , CreateResponse
  , GetAllResponse
  -- , GetByIdResponse
  -- , UpdateByIdResponse
  -- , DeleteByIdResponse
  ) where

import qualified Cascade.Api.Data.Project      as Project
import qualified Cascade.Api.Data.Task         as Task
import           Cascade.Api.Network.Anatomy.Prelude
import qualified Cascade.Api.Servant.Response  as Response
import           Data.Generics.Labels           ( )

type CreateResponse = '[Response.Created Task.Readable]

type GetAllResponse = '[Response.Ok [Task.Readable]]

-- type GetByIdResponse = '[Response.Ok Project.Readable , Response.NotFound]

-- type UpdateByIdResponse = '[Response.Ok Project.Readable , Response.NotFound]

-- type DeleteByIdResponse = '[Response.Ok Project.Readable , Response.NotFound]

data Routes route = Routes
  { create
      :: route :- "projects" :> Capture "id" Project.Id :> "tasks" :> ReqBody '[JSON] Task.Creatable :> Post '[JSON] CreateResponse
  , getAll  :: route :- "projects" :> Capture "id" Project.Id :> "tasks" :> Get '[JSON] GetAllResponse
  -- , getById :: route :- Capture "id" Project.Id :> Get '[JSON] GetByIdResponse
  -- , updateById
  --     :: route :- Capture "id" Project.Id :> ReqBody '[JSON] Project.Updatable :> Patch '[JSON] UpdateByIdResponse
  -- , deleteById
  --     :: route :- Capture "id" Project.Id :> Delete '[JSON] DeleteByIdResponse
  }
  deriving stock Generic
