{-|
Module      : Cascade.Api.Network.Wai.Application
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2022 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.Wai.Application
    ( application
    ) where

import           Cascade.Api.Network.Server
import           Cascade.Api.Servant.Authentication
import qualified Network.Wai                        as Wai
import           Polysemy                           ( Members, Sem )
import           Polysemy.Error                     ( Error )
import           Servant.Server                     ( Context (..), Handler, ServerError )
import           Servant.Server.Experimental.Auth
import           Servant.Server.Generic             ( genericServeTWithContext )

application :: Members (Error ServerError ': Effects) r => (forall a . Sem r a -> Handler a) -> Wai.Application
application nt = genericServeTWithContext nt server context where context = mkAuthHandler handleAuthentication :. EmptyContext
