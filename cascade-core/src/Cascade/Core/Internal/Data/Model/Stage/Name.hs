{-|
Module      : Cascade.Core.Internal.Data.Model.Stage.Name
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Core.Internal.Data.Model.Stage.Name (Name, un, mk, unsafePhaseCoerce) where

import           Cascade.Core.Data.Model.Phase       ( Phase )
import qualified Cascade.Core.Data.Model.Phase      as Phase
import qualified Cascade.Data.Text.Finite           as Text.Finite

newtype Name (phase :: Phase) = Name { un :: Text }

type role Name nominal

mk :: Text -> Either Text.Finite.Error (Name 'Phase.Unknown)
mk = fmap Name . fmap Text.Finite.un . Text.Finite.mk @1 @233

unsafePhaseCoerce :: Name p -> Name p'
unsafePhaseCoerce = coerce
