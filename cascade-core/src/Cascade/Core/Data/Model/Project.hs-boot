module Cascade.Core.Data.Model.Project (Project) where

import           Cascade.Core.Data.Model.Phase       ( Phase )

-- brittany-disable-next-binding
data Project (phase :: Phase)

type role Project nominal
