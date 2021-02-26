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
  , CreateResponse
  , GetAllResponse
  , GetByIdResponse
  , UpdateByIdResponse
  , DeleteByIdResponse
  ) where

import qualified Cascade.Api.Data.Project           as Project
import           Cascade.Api.Network.Anatomy.Prelude
import qualified Cascade.Api.Servant.Response       as Response
import           Data.Generics.Labels                ( )

type CreateResponse = '[Response.Created Project.Readable]

type GetAllResponse = '[Response.Ok [Project.Readable]]

type GetByIdResponse = '[Response.Ok Project.Readable , Response.NotFound]

type UpdateByIdResponse = '[Response.Ok Project.Readable , Response.NotFound]

type DeleteByIdResponse = '[Response.Ok Project.Readable , Response.NotFound]

data Routes route = Routes
  { create     :: route :- ReqBody '[JSON] Project.Creatable :> Post '[JSON] CreateResponse
  , getAll     :: route :- Get '[JSON] GetAllResponse
  , getById    :: route :- Capture "id" Project.Id :> Get '[JSON] GetByIdResponse
  , updateById :: route :- Capture "id" Project.Id :> ReqBody '[JSON] Project.Updatable :> Patch '[JSON] UpdateByIdResponse
  , deleteById :: route :- Capture "id" Project.Id :> Delete '[JSON] DeleteByIdResponse
  }
  deriving stock Generic
