{-|
Module      : Cascade.Api.Network.TestClient.Api.Projects
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.TestClient.Api.Projects
  ( GetByIdResponse
  , UpdateByIdResponse
  , DeleteByIdResponse
  , getById
  , updateById
  , deleteById
  ) where

import qualified Cascade.Api.Data.Project      as Project
import qualified Cascade.Api.Network.Anatomy.Api.Projects
                                               as Api.Projects
import           Cascade.Api.Network.TestClient ( interpret )
import qualified Cascade.Api.Network.TestClient.Api
                                               as Client.Api
import           Control.Lens                   ( (^.) )
import           Data.Generics.Labels           ( )
import           Servant.API                    ( Union )
import           Servant.Client.Free            ( ResponseF )

type GetByIdResponse = (ResponseF (Union Api.Projects.GetByIdResponse))

getById :: Project.Id -> IO GetByIdResponse
getById = interpret . go where go = Client.Api.projects ^. #getById

type UpdateByIdResponse = (ResponseF (Union Api.Projects.UpdateByIdResponse))

updateById :: Project.Id -> Project.Updatable -> IO UpdateByIdResponse
updateById id updatable = interpret $ go id updatable
  where go = Client.Api.projects ^. #updateById

type DeleteByIdResponse = (ResponseF (Union Api.Projects.DeleteByIdResponse))

deleteById :: Project.Id -> IO DeleteByIdResponse
deleteById = interpret . go where go = Client.Api.projects ^. #deleteById
