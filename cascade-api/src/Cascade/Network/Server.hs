module Cascade.Network.Server where

import qualified Cascade.Effect.Database.Project
                                               as Database
                                                ( ProjectL )
import           Cascade.Network.Anatomy
import qualified Cascade.Network.Server.Api    as Api
import           Polysemy                       ( Members
                                                , Sem
                                                )
import           Servant.API.Generic
import           Servant.Server.Generic         ( AsServerT
                                                , genericServerT
                                                )

server :: Members '[Database.ProjectL] r => ToServant Routes (AsServerT (Sem r))
server = genericServerT Routes { api = Api.server }
