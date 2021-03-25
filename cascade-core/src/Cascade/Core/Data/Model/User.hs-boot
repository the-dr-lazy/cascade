module Cascade.Core.Data.Model.User (User) where

import           Cascade.Core.Data                   ( Phase )

-- brittany-disable-next-binding
data User (phase :: Phase)

type role User nominal
