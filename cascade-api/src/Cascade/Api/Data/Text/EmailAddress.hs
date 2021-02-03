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
  , ValidationError(..)
  , un
  , mk
  ) where

import           Control.Lens.TH                ( makeWrapped )
import           Data.Aeson                     ( ToJSON )
import           Text.Email.Validate            ( canonicalizeEmail )

newtype EmailAddress = Mk
  { un :: Text }
  deriving newtype (Show, Eq, ToJSON)

makeWrapped ''EmailAddress

pattern EmailAddress :: Text -> EmailAddress
pattern EmailAddress a <- Mk a
{-# COMPLETE EmailAddress #-}

data ValidationError = IsInvalid
  deriving stock (Generic, Show)
  deriving anyclass ToJSON

mk :: Text -> Maybe EmailAddress
mk = fmap Mk . fmap decodeUtf8 . canonicalizeEmail . encodeUtf8
