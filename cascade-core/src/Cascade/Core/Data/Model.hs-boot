module Cascade.Core.Data.Model (User, Project, Stage, Task, Label) where

import           Cascade.Core.Data.Model.Phase       ( Phase )

-------------------------------------------------------
-- User

-- brittany-disable-next-binding
data User (phase :: Phase)

type role User nominal

-------------------------------------------------------
-- Project

-- brittany-disable-next-binding
data Project (phase :: Phase)

type role Project nominal

-------------------------------------------------------
-- Stage

-- brittany-disable-next-binding
data Stage (phase :: Phase)

type role Stage nominal

-------------------------------------------------------
-- Task

-- brittany-disable-next-binding
data Task (phase :: Phase)

type role Task nominal

-------------------------------------------------------
-- Label

-- brittany-disable-next-binding
data Label (phase :: Phase)

type role Label nominal
