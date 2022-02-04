{-|
Module      : Cascade.Api.Network.Anatomy.Api.Tasks
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2022 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.Anatomy.Api.Tasks
    ( DeleteByIdResponse
    , GetByIdResponse
    , Routes (..)
    , UpdateByIdResponse
    ) where

import qualified Cascade.Api.Data.Task               as Task
import           Cascade.Api.Network.Anatomy.Prelude
import qualified Cascade.Api.Servant.Response        as Response
import qualified Cascade.Data.Validation             as Validation
import           Data.Generics.Labels                ()

type GetByIdResponse = '[Response.Ok Task.Readable , Response.NotFound]

type UpdateByIdResponse = '[Response.Ok Task.Readable , Response.NotFound , Response.Unprocessable (Task.Updatable 'Validation.Error)]

type DeleteByIdResponse = '[Response.Ok Task.Readable , Response.NotFound]

data Routes route = Routes { getById :: route :- Capture "id" Task.Id :> Get '[JSON] GetByIdResponse
                           , updateById :: route :- Capture "id" Task.Id :> ReqBody '[JSON] (Task.Updatable Validation.Raw) :> Patch '[JSON] UpdateByIdResponse
                           , deleteById :: route :- Capture "id" Task.Id :> Delete '[JSON] DeleteByIdResponse
                           }
  deriving stock (Generic)
