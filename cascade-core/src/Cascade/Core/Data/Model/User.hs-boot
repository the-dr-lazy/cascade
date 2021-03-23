module Cascade.Core.Data.Model.User (User) where

import Cascade.Core.Data.Phase (Phase)

data User (phase :: Phase)

type role User nominal
