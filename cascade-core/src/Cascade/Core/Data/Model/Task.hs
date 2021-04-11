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

module Cascade.Core.Data.Model.Task (Task.Id, Task(..), Formation(..), Unit(..), Group(..), Pomodoro(..), Status(..), Title, Description) where

import qualified Cascade.Core.Data.Model.Phase      as Phase
import           Cascade.Core.Internal.Data.Model.Label.Id
                                                    as Label
import           Cascade.Core.Internal.Data.Model.Task.Id
                                                    as Task
import qualified Cascade.Data.List                  as List
import qualified Cascade.Data.Text                  as Text
import           Chronos                             ( Time )

data Task phase = Task
  { id        :: Task.Id phase
  , formation :: Formation phase
  }

type role Task nominal

data Formation phase = Solo (Unit phase) | Grouped (Group phase)

type role Formation nominal

type Title = Text.Finite 1 233
type Description = Text.Finite 1 2584

data Unit phase = Unit
  { title       :: Title
  , description :: Maybe Description
  , pomodoro    :: Pomodoro
  , status      :: Status
  , labels      :: [Label.Id 'Phase.Persisted]
  , deadlineAt  :: Maybe Time
  }

type role Unit nominal

data Group phase = Group
  { title       :: Title
  , description :: Maybe Description
  , units       :: List.AtLeastTwo (Unit phase)
  }

type role Group nominal

data Pomodoro = Pomodoro
  { elapsed :: Word
  , planned :: Maybe Word
  }

data Status = ToDo | Doing | Done
