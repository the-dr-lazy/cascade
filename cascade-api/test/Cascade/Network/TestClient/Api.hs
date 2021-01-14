module Cascade.Network.TestClient.Api
  ( projects
  ) where

import qualified Cascade.Network.Anatomy.Api.Projects
                                               as Api.Projects
import qualified Cascade.Network.TestClient    as Client
import           Control.Lens                   ( (^.) )
import           Control.Monad.Free             ( Free )
import           Servant.API.Generic            ( fromServant )
import           Servant.Client.Free            ( ClientF )
import           Servant.Client.Generic         ( AsClientT )

projects :: Api.Projects.Routes (AsClientT (Free ClientF))
projects = fromServant $ Client.api ^. #projects
