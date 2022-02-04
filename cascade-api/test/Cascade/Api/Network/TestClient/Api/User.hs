{-|
Module      : Cascade.Api.Network.TestClient.Api.User
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2022 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.TestClient.Api.User
    ( projects
    ) where

import qualified Cascade.Api.Network.Anatomy.Api.User.Projects as Api.User.Projects
import qualified Cascade.Api.Network.TestClient.Api            as Client.Api
import           Control.Lens                                  ( (^.) )
import           Control.Monad.Free                            ( Free )
import           Servant.API.Generic                           ( fromServant )
import           Servant.Client.Free                           ( ClientF )
import           Servant.Client.Generic                        ( AsClientT )

projects :: Api.User.Projects.Routes (AsClientT (Free ClientF))
projects = fromServant $ Client.Api.user ^. #projects
