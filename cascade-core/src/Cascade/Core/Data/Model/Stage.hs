{-|
Module      : Cascade.Core.Data.Model.Stage
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Core.Data.Model.Stage (Stage.Id, Stage(..), Name) where

import qualified Cascade.Core.Data.Model.Phase      as Phase
import           Cascade.Core.Data.Model.Stage.Name  ( Name )
import qualified Cascade.Core.Internal.Data.Model.Stage.Id
                                                    as Stage
import qualified Cascade.Core.Internal.Data.Model.Task.Id
                                                    as Task

data Stage phase = Stage
  { id    :: Stage.Id phase
  , name  :: Name phase
  , tasks :: [Task.Id 'Phase.Persisted]
  }

type role Stage nominal
