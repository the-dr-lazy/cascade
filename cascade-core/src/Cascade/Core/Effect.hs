{-|
Module      : Cascade.Core.Effect
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Core.Effect (IdL, TimeL, ScryptL, DatabaseL) where

import           Cascade.Core.Effect.Database        ( DatabaseL )
import           Cascade.Core.Effect.Id              ( IdL )
import           Cascade.Core.Effect.Scrypt          ( ScryptL )
import           Cascade.Core.Effect.Time            ( TimeL )
