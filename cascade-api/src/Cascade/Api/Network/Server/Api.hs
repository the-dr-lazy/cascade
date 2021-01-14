{-|
Module      : Cascade.Api.Network.Server.Api
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.Server.Api
  ( server
  ) where

import qualified Cascade.Api.Effect.Database.Project
                                               as Database
                                                ( ProjectL )
import           Cascade.Api.Network.Anatomy.Api
import qualified Cascade.Api.Network.Server.Api.Projects
                                               as Api.Projects
import           Polysemy                       ( Members
                                                , Sem
                                                )
import           Servant.API.Generic
import           Servant.Server.Generic         ( AsServerT
                                                , genericServerT
                                                )

server :: Members '[Database.ProjectL] r => ToServant Routes (AsServerT (Sem r))
server = genericServerT Routes { projects = Api.Projects.server }
