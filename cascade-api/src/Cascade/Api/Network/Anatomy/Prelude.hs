{-|
Module      : Cascade.Api.Network.Anatomy.Prelude
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.Anatomy.Prelude
  ( module Servant.API
  , module Servant.API.Generic
  , module Cascade.Api.Servant.Authentication
  , Get
  , Post
  , Patch
  , Delete
  ) where

import           Cascade.Api.Servant.Authentication  ( Auth )
import           Servant.API                  hiding ( Delete
                                                     , Get
                                                     , Patch
                                                     , Post
                                                     )
import           Servant.API.Generic

type Get = UVerb 'GET

type Post = UVerb 'POST

type Patch = UVerb 'PATCH

type Delete = UVerb 'DELETE
