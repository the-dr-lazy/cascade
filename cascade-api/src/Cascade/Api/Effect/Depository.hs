{-|
Module      : Cascade.Api.Effect.Depository
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Effect.Depository
  ( ProjectL
  , UserL
  ) where

import           Cascade.Api.Effect.Depository.Project
                                                ( ProjectL )
import           Cascade.Api.Effect.Depository.User
                                                ( UserL )
