{-|
Module      : Cascade.Api.Data.Session
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Data.Session
  ( Session(..)
  ) where

import qualified Cascade.Api.Data.Jwt          as Jwt

data Session
  = Authenticated Jwt.PrivateClaims
  | Anonymous
  deriving stock (Generic, Show, Eq)
