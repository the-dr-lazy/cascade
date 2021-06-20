{-|
Module      : Cascade.Api.Hedgehog.Gen.Id
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Hedgehog.Gen.Id
    ( id
    ) where

import           Cascade.Api.Data.Id
import qualified Cascade.Api.Hedgehog.Gen as Gen
import           Hedgehog

id :: MonadGen m => m (Id entity)
id = Id <$> Gen.uuid
