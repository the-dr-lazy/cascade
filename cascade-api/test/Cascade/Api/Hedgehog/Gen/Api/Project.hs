module Cascade.Api.Hedgehog.Gen.Api.Project
  ( project
  ) where

import           Cascade.Api.Data.Project
import qualified Cascade.Api.Data.Project      as Project
import qualified Cascade.Api.Hedgehog.Gen      as Gen
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

class ProjectGenerator (f :: Type -> Type) where
  project :: MonadGen m => m (f Project)

instance ProjectGenerator Readable where
  project = ProjectR <$> id <*> name
   where
    id   = Gen.uuid |> fmap Project.Id
    name = Gen.text (Range.linear 8 32) Gen.alphaNum

instance ProjectGenerator Creatable where
  project = ProjectC <$> name
    where name = Gen.text (Range.linear 8 32) Gen.alphaNum

instance ProjectGenerator Updatable where
  project = ProjectU <$> name
    where name = Gen.maybe (Gen.text (Range.linear 8 32) Gen.alphaNum)
