{-|
Module      : Cascade.Api.Network.Anatomy.Api.Authentication
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.Anatomy.Api.Authentication (Routes(..), LoginResponse) where

import qualified Cascade.Api.Data.Authentication    as Authentication
import           Cascade.Api.Network.Anatomy.Prelude
import           Data.Generics.Labels                ( )
import           Web.Cookie                          ( SetCookie )

type LoginResponse = Headers '[Header "Set-Cookie" SetCookie , Header "Set-Cookie" SetCookie] NoContent

data Routes route = Routes
    { login :: route :- "login" :> ReqBody '[JSON] Authentication.RawCredential :> Verb 'POST 204 '[JSON] LoginResponse
    }
    deriving stock Generic
