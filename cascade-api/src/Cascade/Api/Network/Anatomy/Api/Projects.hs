module Cascade.Api.Network.Anatomy.Api.Projects
  ( Routes(..)
  , CreateResponse
  , GetAllResponse
  , GetByIdResponse
  , UpdateByIdResponse
  , DeleteByIdResponse
  ) where

import           Cascade.Api.Data.Project
import qualified Cascade.Api.Data.Project      as Project
import           Cascade.Api.Network.Anatomy.Prelude
import qualified Cascade.Api.Servant.Response  as Response
import           Data.Generics.Labels           ( )

type CreateResponse = '[Response.Created (Readable Project)]

type GetAllResponse = '[Response.Ok [Readable Project]]

type GetByIdResponse = '[Response.Ok (Readable Project) , Response.NotFound]

type UpdateByIdResponse = '[Response.Ok (Readable Project) , Response.NotFound]

type DeleteByIdResponse = '[Response.Ok (Readable Project) , Response.NotFound]

data Routes route = Routes
  { create
      :: route :- ReqBody '[JSON] (Creatable Project) :> Post '[JSON] CreateResponse
  , getAll  :: route :- Get '[JSON] GetAllResponse
  , getById :: route :- Capture "id" Project.Id :> Get '[JSON] GetByIdResponse
  , updateById
      :: route :- Capture "id" Project.Id :> ReqBody '[JSON] (Updatable Project) :> Patch '[JSON] UpdateByIdResponse
  , deleteById
      :: route :- Capture "id" Project.Id :> Delete '[JSON] DeleteByIdResponse
  }
  deriving stock Generic