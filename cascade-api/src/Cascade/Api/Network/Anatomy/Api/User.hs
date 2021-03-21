{-|
Module      : Cascade.Api.Network.Anatomy.Api.User
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.Anatomy.Api.User (Routes(..)) where

import qualified Cascade.Api.Network.Anatomy.Api.User.Projects
                                                    as Projects
import           Data.Generics.Labels                ( )
import           Servant
import           Servant.API.Generic

data Routes route = Routes
  { projects :: route :- "projects" :> ToServantApi Projects.Routes
  }
  deriving stock Generic
