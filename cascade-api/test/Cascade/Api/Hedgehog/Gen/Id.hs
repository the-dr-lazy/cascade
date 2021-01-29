module Cascade.Api.Hedgehog.Gen.Id
  ( id
  ) where

import           Cascade.Api.Data.Id
import qualified Cascade.Api.Hedgehog.Gen      as Gen
import           Hedgehog

id :: MonadGen m => m (Id entity)
id = Id <$> Gen.uuid
