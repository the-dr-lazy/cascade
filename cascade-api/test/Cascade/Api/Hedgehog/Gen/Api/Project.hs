{-|
Module      : Cascade.Api.Hedgehog.Gen.Api.Project
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Hedgehog.Gen.Api.Project
  ( project
  ) where

import           Cascade.Api.Data.Project
import qualified Cascade.Api.Hedgehog.Gen.Id   as Gen
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

class ProjectGenerator (f :: Type -> Type) where
  project :: MonadGen m => m (f Project)

instance ProjectGenerator Readable where
  project = ProjectR <$> Gen.id <*> name
    where name = Gen.text (Range.linear 8 32) Gen.alphaNum

instance ProjectGenerator Creatable where
  project = ProjectC <$> name
    where name = Gen.text (Range.linear 8 32) Gen.alphaNum

instance ProjectGenerator Updatable where
  project = ProjectU <$> name
    where name = Gen.maybe (Gen.text (Range.linear 8 32) Gen.alphaNum)
