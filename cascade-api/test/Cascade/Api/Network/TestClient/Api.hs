{-|
Module      : Cascade.Api.Network.TestClient.Api
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.TestClient.Api
  ( projects
  , users
  , authentication
  , user
  ) where

import qualified Cascade.Api.Network.Anatomy.Api.Authentication
                                               as Api.Authentication
import qualified Cascade.Api.Network.Anatomy.Api.Projects
                                               as Api.Projects
import qualified Cascade.Api.Network.Anatomy.Api.User
                                               as Api.User
import qualified Cascade.Api.Network.Anatomy.Api.Users
                                               as Api.Users
import qualified Cascade.Api.Network.TestClient
                                               as Client
import           Control.Lens                   ( (^.) )
import           Control.Monad.Free             ( Free )
import           Servant.API.Generic            ( fromServant )
import           Servant.Client.Free            ( ClientF )
import           Servant.Client.Generic         ( AsClientT )

projects :: Api.Projects.Routes (AsClientT (Free ClientF))
projects = fromServant $ Client.api ^. #projects

users :: Api.Users.Routes (AsClientT (Free ClientF))
users = fromServant $ Client.api ^. #users

authentication :: Api.Authentication.Routes (AsClientT (Free ClientF))
authentication = fromServant $ Client.api ^. #authentication

user :: Api.User.Routes (AsClientT (Free ClientF))
user = fromServant $ Client.api ^. #user
