module Cascade.Api.Network.TestClient.Api.Projects
  ( create
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

create :: Creatable Project
       -> IO (ResponseF (Union Api.Projects.CreateResponse))
create = interpret . go where go = Client.Api.projects ^. #create

getAll :: IO (ResponseF [Readable Project])
getAll = interpret go where go = Client.Api.projects ^. #getAll

getById :: Id -> IO (ResponseF (Union Api.Projects.GetByIdResponse))
getById = interpret . go where go = Client.Api.projects ^. #getById

updateById :: Project.Id
           -> Updatable Project
           -> IO (ResponseF (Union Api.Projects.UpdateByIdResponse))
updateById id updatable = interpret $ go id updatable
  where go = Client.Api.projects ^. #updateById

deleteById :: Project.Id
           -> IO (ResponseF (Union Api.Projects.DeleteByIdResponse))
deleteById = interpret . go where go = Client.Api.projects ^. #deleteById
