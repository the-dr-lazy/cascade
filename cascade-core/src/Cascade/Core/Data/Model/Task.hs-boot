module Cascade.Core.Data.Model.Task (Task, Spec, Unit, Group, Pomodoro, Status) where

import           Cascade.Core.Data                   ( Phase )

-- brittany-disable-next-binding
data Task (phase :: Phase)

type role Task nominal

data Spec

data Unit

data Group

data Pomodoro

data Status
