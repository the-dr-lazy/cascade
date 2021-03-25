{-|
Module      : Cascade.Core.Data.Model.Project
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Core.Data.Model.Project (Project(..)) where

import           Cascade.Core.Data                   ( Id
                                                     , Slug
                                                     )
import {-# SOURCE #-} Cascade.Core.Data.Model.User   ( User )
import           Cascade.Core.Data.Phase             ( Suitable )
import qualified Cascade.Data.Text                  as Text
import           Chronos                             ( Time )
import qualified Relude.List                        as List

data Project phase = Project
  { id        :: Id Project phase
  , slug      :: Slug phase
  , name      :: Text.Finite 1 233
  , users     :: List.NonEmpty (phase `Suitable` Id User)
  , createdAt :: Time
  , updatedAt :: Time
  }
