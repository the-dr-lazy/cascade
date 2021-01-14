module Cascade.Network.Server.Api where

import qualified Cascade.Effect.Database.Project
                                               as Database
                                                ( ProjectL )
import           Cascade.Network.Anatomy.Api
import qualified Cascade.Network.Server.Api.Projects
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
