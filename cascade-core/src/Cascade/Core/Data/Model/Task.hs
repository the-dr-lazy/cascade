{-|
Module      : Cascade.Core.Data.Model.Task
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Core.Data.Model.Task (Task(..), Spec(..), Unit(..), Group(..), Pomodoro(..), Status(..)) where

import           Cascade.Core.Data                   ( Id )
import {-# SOURCE #-} Cascade.Core.Data.Model.Label  ( Label )
import {-# SOURCE #-} Cascade.Core.Data.Model.Stage  ( Stage )
import qualified Cascade.Core.Data.Phase            as Phase
import           Cascade.Core.Data.Phase             ( Suitable )
import qualified Cascade.Data.Text                  as Text
import           Chronos                             ( Time )

data Task phase = Task
  { id    :: Id Task phase
  , stage :: phase `Suitable` Id Stage
  , spec  :: Spec phase
  }

data Spec phase = Solo (Unit phase) | Grouped (Group phase)

data Unit phase = Unit
  { title       :: Text.Finite 1 233
  , description :: Maybe (Text.Finite 1 2584)
  , pomodoro    :: Pomodoro
  , status      :: Status
  , labels      :: [Id Label 'Phase.Persisted]
  , deadlineAt  :: Maybe Time
  , createdAt   :: Time
  , updatedAt   :: Time
  }

data Group phase = Group
  { title       :: Text.Finite 1 233
  , description :: Maybe (Text.Finite 1 2584)
  , units       :: List.AtLeastTwo (Unit phase)
  }

data Pomodoro = Pomodoro
  { elapsed :: Word
  , planned :: Word
  }

data Status = ToDo | Doing | Done
