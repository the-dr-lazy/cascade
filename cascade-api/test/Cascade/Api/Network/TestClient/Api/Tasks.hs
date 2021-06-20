{-|
Module      : Cascade.Api.Network.TestClient.Api.Tasks
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.TestClient.Api.Tasks
    ( DeleteByIdResponse
    , GetByIdResponse
    , UpdateByIdResponse
    , deleteById
    , getById
    , updateById
    ) where

import qualified Cascade.Api.Data.Task                 as Task
import qualified Cascade.Api.Network.Anatomy.Api.Tasks as Api.Tasks
import           Cascade.Api.Network.TestClient        ( interpret )
import qualified Cascade.Api.Network.TestClient.Api    as Client.Api
import qualified Cascade.Data.Validation               as Validation
import           Control.Lens                          ( (^.) )
import           Data.Generics.Labels                  ()
import           Prelude                               hiding ( getAll )
import           Servant.API                           ( Union )
import           Servant.Client.Free                   ( ResponseF )

type GetByIdResponse = (ResponseF (Union Api.Tasks.GetByIdResponse))

type UpdateByIdResponse = (ResponseF (Union Api.Tasks.UpdateByIdResponse))

type DeleteByIdResponse = (ResponseF (Union Api.Tasks.DeleteByIdResponse))

getById :: Task.Id -> IO GetByIdResponse
getById = interpret . go where go = Client.Api.tasks ^. #getById

updateById :: Task.Id -> Task.Updatable 'Validation.Raw -> IO UpdateByIdResponse
updateById id updatable = interpret $ go id updatable where go = Client.Api.tasks ^. #updateById

deleteById :: Task.Id -> IO DeleteByIdResponse
deleteById = interpret . go where go = Client.Api.tasks ^. #deleteById
