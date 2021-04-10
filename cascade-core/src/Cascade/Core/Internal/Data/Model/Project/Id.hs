{-|
Module      : Cascade.Core.Internal.Data.Model.Project.Id
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Core.Internal.Data.Model.Project.Id (Id, unsafeMk, un, unsafePhaseCoerce) where

import           Cascade.Core.Data.Model.Phase       ( Phase )

newtype Id (phase :: Phase) = Mk { un :: UUID }

unsafeMk :: UUID -> Id p
unsafeMk = Mk

unsafePhaseCoerce :: Id p -> Id p'
unsafePhaseCoerce = coerce
