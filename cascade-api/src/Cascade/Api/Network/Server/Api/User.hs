{-|
Module      : Cascade.Api.Network.Server.Api.User
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.Server.Api.User (server) where

import qualified Cascade.Api.Effect.Database.Project
                                                    as Database
import           Cascade.Api.Network.Anatomy.Api.User
import qualified Cascade.Api.Network.Server.Api.User.Projects
                                                    as Projects
import           Polysemy
import           Servant.API.Generic
import           Servant.Server.Generic

server :: Member Database.ProjectL r => ToServant Routes (AsServerT (Sem r))
server = genericServerT Routes { projects = Projects.server }
