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
    ( DeleteByIdResponse
    , GetByIdResponse
    , Routes (..)
    , UpdateByIdResponse
    ) where

import qualified Cascade.Api.Data.Project                       as Project
import qualified Cascade.Api.Network.Anatomy.Api.Projects.Tasks as Api.Projects.Tasks
import           Cascade.Api.Network.Anatomy.Prelude
import qualified Cascade.Api.Servant.Response                   as Response
import           Data.Generics.Labels                           ()

type GetByIdResponse = '[Response.Unauthorized , Response.NotFound , Response.Ok Project.Readable]

type UpdateByIdResponse = '[Response.Unauthorized , Response.NotFound , Response.Ok Project.Readable]

type DeleteByIdResponse = '[Response.Unauthorized , Response.NotFound , Response.Ok Project.Readable]

data Routes route = Routes { deleteById :: route :- Capture "id" Project.Id :> Auth :> Delete '[JSON] DeleteByIdResponse
                           , getById :: route :- Capture "id" Project.Id :> Auth :> Get '[JSON] GetByIdResponse
                           , tasks :: route :- Capture "id" Project.Id :> "tasks" :> ToServantApi Api.Projects.Tasks.Routes
                           , updateById :: route :- Capture "id" Project.Id :> ReqBody '[JSON] Project.Updatable :> Auth :> Patch '[JSON] UpdateByIdResponse
                           }
  deriving stock (Generic)
