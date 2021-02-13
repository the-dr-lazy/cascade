module Cascade.Api.Network.Anatomy.Api.Projects.Tasks
  ( Routes(..)
  , CreateResponse
  , FindByProjectIdResponse
  )
where

import qualified Cascade.Api.Data.Task         as Task
import           Cascade.Api.Network.Anatomy.Prelude
import qualified Cascade.Api.Servant.Response  as Response
import           Data.Generics.Labels           ( )

type CreateResponse
  = '[Response.Created Task.Readable, Response.Unprocessable
    Task.RawCreatableValidationErrors]

type FindByProjectIdResponse = '[Response.Ok [Task.Readable]]

data Routes route = Routes
  { create :: route :- ReqBody '[JSON] Task.RawCreatable :> Post '[JSON] CreateResponse
  , findByProjectId :: route :- Get '[JSON] FindByProjectIdResponse
  }
  deriving stock Generic
