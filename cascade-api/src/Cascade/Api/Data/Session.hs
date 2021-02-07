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
  , withAnonymous
  , withAuthenticated
  ) where

import qualified Cascade.Api.Data.Jwt          as Jwt
import qualified Cascade.Api.Servant.Response  as Response
import           Servant                        ( IsMember
                                                , Union
                                                , respond
                                                )

data Session
  = Authenticated Jwt.PrivateClaims
  | Anonymous
  deriving stock (Generic, Show, Eq)

withAnonymous :: Applicative m
              => IsMember Response.Forbidden as
              => m (Union as)
              -> Session
              -> m (Union as)
withAnonymous m Anonymous         = m
withAnonymous _ (Authenticated _) = respond Response.Forbidden

withAuthenticated :: Applicative m
                  => IsMember Response.Unauthorized as
                  => (Jwt.PrivateClaims -> m (Union as))
                  -> Session
                  -> m (Union as)
withAuthenticated m (Authenticated x) = m x
withAuthenticated _ Anonymous         = respond Response.Unauthorized
