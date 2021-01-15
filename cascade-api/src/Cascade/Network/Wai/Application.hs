module Cascade.Network.Wai.Application
  ( application
  ) where

import qualified Cascade.Effect.Database.Project
                                               as Database
import           Cascade.Network.Server
import qualified Network.Wai                   as Wai
import           Polysemy                       ( Members
                                                , Sem
                                                )
import           Polysemy.Error                 ( Error )
import           Servant.Server                 ( Handler
                                                , ServerError
                                                )
import           Servant.Server.Generic         ( genericServeT )

type Effects = '[Database.ProjectL , Error ServerError]

application :: Members Effects r
            => (forall a . Sem r a -> Handler a)
            -> Wai.Application
application nt = genericServeT nt server
