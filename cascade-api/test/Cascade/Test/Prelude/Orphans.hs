{-# OPTIONS_GHC -Wno-orphans #-}

module Cascade.Test.Prelude.Orphans
  () where

import qualified Network.HTTP.Types.Status     as Http
                                                ( Status(..) )

deriving stock instance Generic Http.Status
