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
    , Error (..)
    , mk
    , parse
    , pattern EmailAddress
    , un
    ) where

import qualified Cascade.Api.Data.Aeson.FieldErrorFormat as Aeson
import           Cascade.Data.Validation                 (Validation)
import qualified Cascade.Data.Validation                 as Validation
import           Control.Lens.TH                         (makeWrapped)
import           Data.Aeson                              (FromJSON, ToJSON)
import           Text.Email.Validate                     (canonicalizeEmail)

newtype EmailAddress
  = Mk { un :: Text }
  deriving newtype (Eq, FromJSON, Show, ToJSON)

makeWrapped ''EmailAddress

pattern EmailAddress :: Text -> EmailAddress
pattern EmailAddress a <- Mk a
{-# COMPLETE EmailAddress #-}

data Error = IsInvalid deriving stock (Generic, Show)
  deriving (ToJSON) via Aeson.FieldErrorFormat Error
  deriving (FromJSON) via Aeson.FieldErrorFormat Error

type Errors = NonEmpty Error

type instance Validation.Errors Text EmailAddress = Errors

mk :: Text -> Maybe EmailAddress
mk = fmap Mk . fmap decodeUtf8 . canonicalizeEmail . encodeUtf8

parse :: Text -> Validation Errors EmailAddress
parse = Validation.maybeToSuccess (IsInvalid :| []) . mk
