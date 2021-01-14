{-|
Module      : Cascade.Api.Network.Wai.Application
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.Wai.Application
  ( application
  ) where

import qualified Cascade.Api.Effect.Database.Project
                                               as Database
import           Cascade.Api.Network.Server
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
