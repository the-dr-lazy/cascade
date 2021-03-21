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
                                                    as Authentication
import qualified Cascade.Api.Network.Anatomy.Api.Projects
                                                    as Projects
import qualified Cascade.Api.Network.Anatomy.Api.Tasks
                                                    as Tasks
import qualified Cascade.Api.Network.Anatomy.Api.User
                                                    as User
import qualified Cascade.Api.Network.Anatomy.Api.Users
                                                    as Users
import           Data.Generics.Labels                ( )
import           Servant
import           Servant.API.Generic

data Routes route = Routes
  { authentication :: route :- "authentication" :> ToServantApi Authentication.Routes
  , projects       :: route :- "projects" :> ToServantApi Projects.Routes
  , tasks          :: route :- "tasks" :> ToServantApi Tasks.Routes
  , user           :: route :- "user" :> ToServantApi User.Routes
  , users          :: route :- "users" :> ToServantApi Users.Routes
  }
  deriving stock Generic
