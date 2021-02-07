{-|
Module      : Cascade.Api.Network.Anatomy.Api.User.Projects
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.Anatomy.Api.User.Projects
  ( Routes(..)
  , CreateResponse
  , GetAllResponse
  ) where

import qualified Cascade.Api.Data.Project      as Project
import           Cascade.Api.Network.Anatomy.Prelude
import qualified Cascade.Api.Servant.Response  as Response
import           Data.Generics.Labels           ( )

type CreateResponse
  = '[Response.Created Project.Readable , Response.Unauthorized]

type GetAllResponse = '[Response.Ok [Project.Readable] , Response.Unauthorized]

data Routes route = Routes
  { create
      :: route :- ReqBody '[JSON] Project.Creatable :> Auth :> Post '[JSON] CreateResponse
  , getAll :: route :- Auth :> Get '[JSON] GetAllResponse
  }
  deriving stock Generic
