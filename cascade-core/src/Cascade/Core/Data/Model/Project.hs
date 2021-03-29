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

module Cascade.Core.Data.Model.Project (Project(..), Slug, Name) where

import {-# SOURCE #-} Cascade.Core.Data.Model        ( Stage
                                                     , User
                                                     )
import           Cascade.Core.Data.Model.Id          ( Id )
import qualified Cascade.Core.Data.Model.Phase      as Phase
import           Cascade.Core.Data.Model.Project.Slug
                                                     ( Slug )
import qualified Cascade.Data.Text                  as Text
import           Chronos                             ( Time )
import qualified Relude.List                        as List

type Name = Text.Finite 1 233

data Project phase = Project
  { id        :: Project `Id` phase
  , slug      :: Slug phase
  , name      :: Name
  , users     :: List.NonEmpty (User `Id` 'Phase.Persisted)
  , stages    :: List.NonEmpty (Stage `Id` 'Phase.Persisted)
  , createdAt :: Time
  , updatedAt :: Time
  }

type role Project nominal
