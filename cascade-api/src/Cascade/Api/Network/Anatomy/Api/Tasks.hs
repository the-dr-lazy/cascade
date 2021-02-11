module Cascade.Api.Network.Anatomy.Api.Tasks
  ( Routes(..)
  , GetByIdResponse
  )
where

import qualified Cascade.Api.Data.Project      as Project
import qualified Cascade.Api.Data.Task         as Task
import           Cascade.Api.Network.Anatomy.Prelude
import qualified Cascade.Api.Servant.Response  as Response
import           Data.Generics.Labels           ( )

type GetByIdResponse = '[Response.Ok Task.Readable, Response.NotFound]

data Routes route = Routes
  { getById :: route :- Capture "id" Task.Id :> Get '[JSON] GetByIdResponse
  }
  deriving stock Generic
