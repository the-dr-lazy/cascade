{-|
Module      : Cascade.Api.Network.Anatomy.Api.Projects
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.Anatomy.Api.Projects
  ( Routes(..)
  , GetByIdResponse
  , UpdateByIdResponse
  , DeleteByIdResponse
  ) where

import qualified Cascade.Api.Data.Project      as Project
import           Cascade.Api.Network.Anatomy.Prelude
import qualified Cascade.Api.Servant.Response  as Response
import           Data.Generics.Labels           ( )

type GetByIdResponse
  = '[ Response.Unauthorized
     , Response.Forbidden
     , Response.NotFound
     , Response.Ok Project.Readable
     ]

type UpdateByIdResponse
  = '[ Response.Unauthorized
     , Response.Forbidden
     , Response.NotFound
     , Response.Ok Project.Readable
     ]

type DeleteByIdResponse
  = '[ Response.Unauthorized
     , Response.Forbidden
     , Response.NotFound
     , Response.Ok Project.Readable
     ]

data Routes route = Routes
  { getById :: route :- Capture "id" Project.Id :> Get '[JSON] GetByIdResponse
  , updateById
      :: route :- Capture "id" Project.Id :> ReqBody '[JSON] Project.Updatable :> Patch '[JSON] UpdateByIdResponse
  , deleteById
      :: route :- Capture "id" Project.Id :> Delete '[JSON] DeleteByIdResponse
  }
  deriving stock Generic
