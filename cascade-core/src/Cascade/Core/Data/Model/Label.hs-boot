module Cascade.Core.Data.Model.Label (Label) where

import           Cascade.Core.Data                   ( Phase )

-- brittany-disable-next-binding
data Label (phase :: Phase)

type role Label nominal
