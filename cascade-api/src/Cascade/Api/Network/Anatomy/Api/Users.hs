module Cascade.Api.Network.Anatomy.Api.Users
  ( Routes(..)
  , CreateResponse
  ) where

import qualified Cascade.Api.Data.User         as User
import           Cascade.Api.Network.Anatomy.Prelude
import qualified Cascade.Api.Servant.Response  as Response
import           Data.Generics.Labels           ( )

type CreateResponse
  = '[ Response.Created User.Readable
     , Response.Unprocessable User.RawCreatableValidationErrors
     ]

data Routes route
  = Route
    { create
        :: route :- ReqBody '[JSON] User.RawCreatable :> Post '[JSON] CreateResponse
    }
  deriving stock Generic
