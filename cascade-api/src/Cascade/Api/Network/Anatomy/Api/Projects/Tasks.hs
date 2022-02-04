{-|
Module      : Cascade.Api.Network.Anatomy.Api.Projects.Tasks
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2022 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.Anatomy.Api.Projects.Tasks
    ( CreateResponse
    , GetAllByProjectIdResponse
    , Routes (..)
    ) where

import qualified Cascade.Api.Data.Task               as Task
import           Cascade.Api.Network.Anatomy.Prelude
import qualified Cascade.Api.Servant.Response        as Response
import qualified Cascade.Data.Validation             as Validation
import           Data.Generics.Labels                ()

type CreateResponse = '[Response.Created Task.Readable , Response.Unprocessable (Task.Creatable 'Validation.Error) , Response.NotFound]

type GetAllByProjectIdResponse = '[Response.Ok [Task.Readable] , Response.NotFound]

data Routes route = Routes { create :: route :- ReqBody '[JSON] (Task.Creatable Validation.Raw) :> Post '[JSON] CreateResponse
                           , getAllByProjectId :: route :- Get '[JSON] GetAllByProjectIdResponse
                           }
  deriving stock (Generic)
