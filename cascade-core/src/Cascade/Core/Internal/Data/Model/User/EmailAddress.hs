{-|
Module      : Cascade.Core.Internal.Data.Model.User.EmailAddress
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Core.Internal.Data.Model.User.EmailAddress (EmailAddress, pattern EmailAddress, un, mk, unsafePhaseCoerce) where

import           Cascade.Core.Data.Model.Phase       ( Phase )
import qualified Cascade.Core.Data.Model.Phase      as Phase
import           Text.Email.Validate                 ( canonicalizeEmail )

newtype EmailAddress (phase :: Phase) = Mk { un :: Text }
  deriving newtype (Show, Eq)

type role EmailAddress nominal

pattern EmailAddress :: Text -> EmailAddress phase
pattern EmailAddress a <- Mk a
{-# COMPLETE EmailAddress #-}

mk :: Text -> Maybe (EmailAddress 'Phase.Unknown)
mk = fmap Mk . fmap decodeUtf8 . canonicalizeEmail . encodeUtf8

unsafePhaseCoerce :: EmailAddress p -> EmailAddress p'
unsafePhaseCoerce = coerce
