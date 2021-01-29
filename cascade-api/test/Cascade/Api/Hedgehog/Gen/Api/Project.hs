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

import qualified Cascade.Api.Data.Project      as Project
import qualified Cascade.Api.Hedgehog.Gen      as Gen
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

class ProjectGenerator (f :: Type -> Type) where
  project :: MonadGen m => m (f Project)

instance ProjectGenerator Project.Readable where
  project = Project.Readable <$> id <*> name
   where
    id   = Gen.uuid |> fmap Project.Id
    name = Gen.text (Range.linear 8 32) Gen.alphaNum

instance ProjectGenerator Project.Creatable where
  project = Project.Creatable <$> name
    where name = Gen.text (Range.linear 8 32) Gen.alphaNum

instance ProjectGenerator Project.Updatable where
  project = Project.Updatable <$> name
    where name = Gen.maybe (Gen.text (Range.linear 8 32) Gen.alphaNum)
