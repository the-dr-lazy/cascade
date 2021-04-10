module Cascade.Core.Data.Model.Task (Task) where

import           Cascade.Core.Data.Model.Phase       ( Phase )

-- brittany-disable-next-binding
data Task (phase :: Phase)

type role Task nominal
