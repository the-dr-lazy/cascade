module Cascade.Api.Network.TestClient.Api.Users
  ( CreateResponse
  , create
  ) where

import qualified Cascade.Api.Data.User         as User
import qualified Cascade.Api.Network.Anatomy.Api.Users
                                               as Api.Users
import           Cascade.Api.Network.TestClient ( interpret )
import qualified Cascade.Api.Network.TestClient.Api
                                               as Client.Api
import           Control.Lens                   ( (^.) )
import           Data.Generics.Labels           ( )
import           Servant.API                    ( Union )
import           Servant.Client.Free            ( ResponseF )

type CreateResponse = ResponseF (Union Api.Users.CreateResponse)

create :: User.RawCreatable -> IO CreateResponse
create = interpret . go where go = Client.Api.users ^. #create
