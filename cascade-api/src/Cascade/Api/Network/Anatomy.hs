{-|
Module      : Cascade.Api.Network.Anatomy
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2022 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.Anatomy
    ( Routes (..)
    ) where


import qualified Cascade.Api.Network.Anatomy.Api as Api
import           Data.Generics.Labels            ()
import           Servant
import           Servant.API.Generic

data Routes route = Routes { api :: route :- "api" :> ToServantApi Api.Routes
                           }
  deriving stock (Generic)
