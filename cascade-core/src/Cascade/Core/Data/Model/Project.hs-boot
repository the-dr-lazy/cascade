module Cascade.Core.Data.Model.Project (Project) where

import           Cascade.Core.Data                   ( Phase )

-- brittany-disable-next-binding
data Project (phase :: Phase)

type role Project nominal
