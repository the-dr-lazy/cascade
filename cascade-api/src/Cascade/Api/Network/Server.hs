{-|
Module      : Cascade.Api.Network.Server
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.Server
  ( server
  ) where

import qualified Cascade.Api.Effect.Database.Project
                                               as Database
                                                ( ProjectL )
import           Cascade.Api.Network.Anatomy
import qualified Cascade.Api.Network.Server.Api    as Api
import           Polysemy                       ( Members
                                                , Sem
                                                )
import           Servant.Server.Generic         ( AsServerT )

server :: Members '[Database.ProjectL] r => Routes (AsServerT (Sem r))
server = Routes { api = Api.server }
