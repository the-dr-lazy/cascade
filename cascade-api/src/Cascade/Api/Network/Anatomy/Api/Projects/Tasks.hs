{-|
Module      : Cascade.Api.Network.Anatomy.Api.Projects.Tasks
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.Anatomy.Api.Projects.Tasks (Routes(..), CreateResponse, GetAllByProjectIdResponse) where

import qualified Cascade.Api.Data.Task              as Task
import           Cascade.Api.Network.Anatomy.Prelude
import qualified Cascade.Api.Servant.Response       as Response
import           Data.Generics.Labels                ( )

type CreateResponse = '[Response.Created Task.Readable , Response.Unprocessable Task.RawCreatableValidationErrors , Response.NotFound]

type GetAllByProjectIdResponse = '[Response.Ok [Task.Readable] , Response.NotFound]

data Routes route = Routes
  { create            :: route :- ReqBody '[JSON] Task.RawCreatable :> Post '[JSON] CreateResponse
  , getAllByProjectId :: route :- Get '[JSON] GetAllByProjectIdResponse
  }
  deriving stock Generic
