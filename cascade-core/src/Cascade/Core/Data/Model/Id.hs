{-|
Module      : Cascade.Core.Data.Id
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Core.Data.Model.Id (Id) where

import           Cascade.Core.Data.Model.Phase       ( Phase )

data family (entity :: k) `Id` (phase :: Phase)