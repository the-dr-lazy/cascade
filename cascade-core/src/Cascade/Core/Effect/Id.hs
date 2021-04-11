{-|
Module      : Cascade.Core.Effect.Id
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Core.Effect.Id (IdL, next) where

import qualified Cascade.Core.Data.Model.Phase      as Phase
import           Polysemy

data IdL id m a where
  Next ::IdL entity m (id 'Phase.New)

makeSem ''IdL
