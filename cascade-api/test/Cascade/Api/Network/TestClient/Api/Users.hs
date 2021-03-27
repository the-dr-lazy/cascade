{-|
Module      : Cascade.Api.Network.TestClient.Api.Users
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.TestClient.Api.Users (CreateResponse, create) where

import qualified Cascade.Api.Data.User              as User
import qualified Cascade.Api.Network.Anatomy.Api.Users
                                                    as Api.Users
import           Cascade.Api.Network.TestClient      ( interpret )
import qualified Cascade.Api.Network.TestClient.Api as Client.Api
import qualified Cascade.Data.Validation            as Validation
import           Control.Lens                        ( (^.) )
import           Data.Generics.Labels                ( )
import           Servant.API                         ( Union )
import           Servant.Client.Free                 ( ResponseF )

type CreateResponse = ResponseF (Union Api.Users.CreateResponse)

create :: User.Creatable 'Validation.Raw -> IO CreateResponse
create = interpret . go where go = Client.Api.users ^. #create
