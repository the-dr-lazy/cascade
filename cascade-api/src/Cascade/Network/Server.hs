module Cascade.Network.Server
  ( server
  ) where

import qualified Cascade.Effect.Database.Project
                                               as Database
                                                ( ProjectL )
import           Cascade.Network.Anatomy
import qualified Cascade.Network.Server.Api    as Api
import           Polysemy                       ( Members
                                                , Sem
                                                )
import           Servant.Server.Generic         ( AsServerT )

server :: Members '[Database.ProjectL] r => Routes (AsServerT (Sem r))
server = Routes { api = Api.server }
