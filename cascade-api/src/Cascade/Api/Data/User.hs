{-|
Module      : Cascade.Api.Data.User
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Data.User
  ( RawCreatable(..)
  , ParsedCreatable
  , parseRawCreatableUser
  ) where

import           Cascade.Api.Data.ByteString.Password
                                                ( Password )
import qualified Cascade.Api.Data.ByteString.Password
                                               as Password
import           Cascade.Api.Data.Text.EmailAddress
                                                ( EmailAddress )
import qualified Cascade.Api.Data.Text.EmailAddress
                                               as EmailAddress
import           Cascade.Api.Data.Text.Username ( Username )
import qualified Cascade.Api.Data.Text.Username
                                               as Username
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Generics.Labels           ( )
import           Validation

data RawCreatable = RawCreatable
  { username     :: Text
  , emailAddress :: Text
  , password     :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data ParsedCreatable = ParsedCreatable
  { username     :: Username
  , emailAddress :: EmailAddress
  , password     :: Password
  }
  deriving stock (Generic, Show, Eq)

data RawCreatableValidationError
  = UsernameField Username.ValidationErrors
  | PasswordField Password.ValidationErrors
  | EmailAddressField

type RawCreatableValidationErrors = NonEmpty RawCreatableValidationError

parseRawCreatableUser :: RawCreatable
                      -> Validation
                           RawCreatableValidationErrors
                           ParsedCreatable
parseRawCreatableUser RawCreatable {..} =
  let validateUsername = first (pure . UsernameField) $ Username.mk username
      validateEmailAddress =
        maybeToSuccess (pure EmailAddressField) (EmailAddress.mk emailAddress)
      validatePassword =
        first (pure . PasswordField) . Password.mk . encodeUtf8 $ password
  in  ParsedCreatable
        <$> validateUsername
        <*> validateEmailAddress
        <*> validatePassword
