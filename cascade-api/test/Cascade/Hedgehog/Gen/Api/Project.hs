module Cascade.Hedgehog.Gen.Api.Project
  ( project
  ) where

import           Cascade.Data.Api.Project
import qualified Cascade.Data.Api.Project      as Project
import qualified Cascade.Hedgehog.Gen          as Gen
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

class ProjectGenerator (f :: Type -> Type) where
  project :: MonadGen m => m (f Project)

instance ProjectGenerator Readable where
  project = ProjectR <$> id <*> name
   where
    id   = Gen.uuid |> fmap Project.Id
    name = Gen.text (Range.linear 8 32) Gen.unicodeAll

instance ProjectGenerator Creatable where
  project = ProjectC <$> name
    where name = Gen.text (Range.linear 8 32) Gen.unicodeAll

instance ProjectGenerator Updatable where
  project = ProjectU <$> name
    where name = Gen.maybe (Gen.text (Range.linear 8 32) Gen.unicodeAll)
