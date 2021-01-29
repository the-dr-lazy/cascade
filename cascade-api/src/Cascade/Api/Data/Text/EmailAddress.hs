{-|
Module      : Cascade.Api.Data.Text.EmailAddress
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Data.Text.EmailAddress
  ( EmailAddress
  , pattern EmailAddress
  , un
  , mk
  ) where

import           Text.Email.Validate            ( canonicalizeEmail )

newtype EmailAddress = Mk
  { un :: Text }
  deriving newtype (Show, Eq)

pattern EmailAddress :: Text -> EmailAddress
pattern EmailAddress a <- Mk a

mk :: Text -> Maybe EmailAddress
mk = fmap Mk . fmap decodeUtf8 . canonicalizeEmail . encodeUtf8
