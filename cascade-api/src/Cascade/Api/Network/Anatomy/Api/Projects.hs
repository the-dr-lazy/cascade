module Cascade.Api.Network.Anatomy.Api.Projects
  ( Routes(..)
  , CreateResponse
  , GetByIdResponse
  , UpdateByIdResponse
  , DeleteByIdResponse
  ) where

import           Cascade.Api.Data.Project
import qualified Cascade.Api.Data.Project      as Project
import qualified Cascade.Api.Servant.Resource      as Resource
import           Data.Generics.Labels           ( )
import           Servant
import           Servant.API.Generic

type CreateResponse = '[Resource.Created (Readable Project)]

type GetByIdResponse = '[Resource.Ok (Readable Project) , Resource.NotFound]

type UpdateByIdResponse = '[Resource.Ok (Readable Project) , Resource.NotFound]

type DeleteByIdResponse = '[Resource.Ok (Readable Project) , Resource.NotFound]

data Routes route = Routes
  { create
      :: route :- ReqBody '[JSON] (Creatable Project) :> UVerb 'POST '[JSON] CreateResponse
  , getAll :: route :- Get '[JSON] [Readable Project]
  , getById
      :: route :- Capture "id" Project.Id :> UVerb 'GET '[JSON] GetByIdResponse
  , updateById
      :: route :- Capture "id" Project.Id :> ReqBody '[JSON] (Updatable Project) :> UVerb 'PUT '[JSON] UpdateByIdResponse
  , deleteById
      :: route :- Capture "id" Project.Id :> UVerb 'DELETE '[JSON] DeleteByIdResponse
  }
  deriving stock Generic
