{-|
Module      : Cascade.Api.Network.Anatomy.Api
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.Anatomy.Api (Routes(..)) where

import qualified Cascade.Api.Network.Anatomy.Api.Authentication
                                                    as Api.Authentication
import qualified Cascade.Api.Network.Anatomy.Api.Projects
                                                    as Api.Projects
import qualified Cascade.Api.Network.Anatomy.Api.Users
                                               as Api.Users
import qualified Cascade.Api.Network.Anatomy.Api.Tasks
                                               as Api.Tasks
import           Data.Generics.Labels           ( )
import           Servant
import           Servant.API.Generic

data Routes route = Routes
  { projects :: route :- "projects" :> ToServantApi Api.Projects.Routes
  , users    :: route :- "users" :> ToServantApi Api.Users.Routes
  , authentication
      :: route :- "authentication" :> ToServantApi Api.Authentication.Routes
  , tasks :: route :- ToServantApi Api.Tasks.Routes
  }
  deriving stock Generic
