{-|
Module      : Cascade.Api.Network.Anatomy.Api.Tasks
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.Anatomy.Api.Tasks (Routes(..), GetByIdResponse, UpdateByIdResponse, DeleteByIdResponse) where

import qualified Cascade.Api.Data.Task              as Task
import           Cascade.Api.Network.Anatomy.Prelude
import qualified Cascade.Api.Servant.Response       as Response
import           Data.Generics.Labels                ( )

type GetByIdResponse = '[Response.Ok Task.Readable, Response.NotFound]

type UpdateByIdResponse = '[Response.Ok Task.Readable, Response.NotFound, Response.Unprocessable Task.RawUpdatableValidationErrors]

type DeleteByIdResponse = '[Response.Ok Task.Readable, Response.NotFound]

data Routes route = Routes
  { getById :: route :- Capture "id" Task.Id :> Get '[JSON] GetByIdResponse
  , updateById
      :: route :- Capture "id" Task.Id :> ReqBody '[JSON] Task.RawUpdatable :> Patch '[JSON] UpdateByIdResponse
  , deleteById
      :: route :- Capture "id" Task.Id :> Delete '[JSON] DeleteByIdResponse
  }
  deriving stock Generic
