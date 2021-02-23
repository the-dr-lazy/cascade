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

module Cascade.Api.Data.Text.EmailAddress (EmailAddress, pattern EmailAddress, ValidationError(..), un, mk) where

import           Cascade.Data.Validation
import qualified Cascade.Data.Validation            as Validation
import           Control.Lens.TH                     ( makeWrapped )
import           Data.Aeson                          ( FromJSON
                                                     , ToJSON
                                                     )
import           Data.Data                           ( Data )
import           Text.Email.Validate                 ( canonicalizeEmail )

newtype EmailAddress = Mk
  { un :: Text }
  deriving newtype (Show, Eq, FromJSON, ToJSON)

makeWrapped ''EmailAddress

pattern EmailAddress :: Text -> EmailAddress
pattern EmailAddress a <- Mk a
{-# COMPLETE EmailAddress #-}

data ValidationError = IsInvalid
  deriving stock (Generic, Data, Show)
  deriving ToJSON via (ApiErrorFormat ValidationError)

instance Validation.ToMessage ValidationError where
  toMessage IsInvalid = "invalid email address"

type ValidationErrors = NonEmpty ValidationError

mk :: Text -> Maybe EmailAddress
mk = fmap Mk . fmap decodeUtf8 . canonicalizeEmail . encodeUtf8

instance Validatable EmailAddress where
  type Raw EmailAddress = Text
  type Errors EmailAddress = ValidationErrors

  validate = pure . maybeToSuccess (IsInvalid :| []) . mk
