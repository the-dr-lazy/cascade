module Cascade.Core.Data.Model.Stage (Stage) where

import           Cascade.Core.Data.Model.Phase       ( Phase )

-- brittany-disable-next-binding
data Stage (phase :: Phase)

type role Stage nominal
