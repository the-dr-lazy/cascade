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

module Cascade.Core.Data.Model.Stage (Stage(..)) where

import           Cascade.Core.Data                   ( Id )
import {-# SOURCE #-} Cascade.Core.Data.Model.Project
                                                     ( Project )
import {-# SOURCE #-} Cascade.Core.Data.Model.Task   ( Task )
import           Cascade.Core.Data.Phase             ( Suitable )
import qualified Cascade.Data.Text                  as Text
import           Chronos                             ( Time )

data Stage phase = Stage
  { id        :: Id Stage phase
  , name      :: Text.Finite 1 233
  , project   :: phase `Suitable` Id Project
  , tasks     :: [phase `Suitable` Task]
  , createdAt :: Time
  , updatedAt :: Time
  }
