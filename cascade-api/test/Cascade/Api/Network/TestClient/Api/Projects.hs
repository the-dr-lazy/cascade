module Cascade.Api.Network.TestClient.Api.Projects
  ( CreateResponse
  , GetAllResponse
  , GetByIdResponse
  , UpdateByIdResponse
  , DeleteByIdResponse
  , create
  , getAll
  , getById
  , updateById
  , deleteById
  ) where

import           Cascade.Api.Data.Project
import qualified Cascade.Api.Data.Project      as Project
import qualified Cascade.Api.Network.Anatomy.Api.Projects
                                               as Api.Projects
import           Cascade.Api.Network.TestClient ( interpret )
import qualified Cascade.Api.Network.TestClient.Api
                                               as Client.Api
import           Control.Lens                   ( (^.) )
import           Data.Generics.Labels           ( )
import           Prelude                 hiding ( getAll )
import           Servant.API                    ( Union )
import           Servant.Client.Free            ( ResponseF )

type CreateResponse = (ResponseF (Union Api.Projects.CreateResponse))

type GetAllResponse = (ResponseF (Union Api.Projects.GetAllResponse))

type GetByIdResponse = (ResponseF (Union Api.Projects.GetByIdResponse))

type UpdateByIdResponse = (ResponseF (Union Api.Projects.UpdateByIdResponse))

type DeleteByIdResponse = (ResponseF (Union Api.Projects.DeleteByIdResponse))

create :: Creatable Project -> IO CreateResponse
create = interpret . go where go = Client.Api.projects ^. #create

getAll :: IO GetAllResponse
getAll = interpret go where go = Client.Api.projects ^. #getAll

getById :: Id -> IO GetByIdResponse
getById = interpret . go where go = Client.Api.projects ^. #getById

updateById :: Project.Id -> Updatable Project -> IO UpdateByIdResponse
updateById id updatable = interpret $ go id updatable
  where go = Client.Api.projects ^. #updateById

deleteById :: Project.Id -> IO DeleteByIdResponse
deleteById = interpret . go where go = Client.Api.projects ^. #deleteById