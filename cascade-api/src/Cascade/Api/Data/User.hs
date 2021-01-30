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
  ( User
  , Id
  , Readable
  , RawCreatableV(..)
  , RawCreatable
  , RawCreatableValidationErrors
  , ParsedCreatable
  , parseRawCreatableUser
  ) where

import           Cascade.Api.Data.ByteString.Password
                                                ( Password )
import qualified Cascade.Api.Data.ByteString.Password
                                               as Password
import qualified Cascade.Api.Data.Id           as Data
import           Cascade.Api.Data.Prelude
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
import           Data.Monoid.Generic
import           Validation

data User

type Id = Data.Id User

data Readable = Readable
  { id           :: Id
  , username     :: Username
  , emailAddress :: EmailAddress
  }
  deriving stock (Generic, Show, Eq)

data RawCreatableV f = RawCreatable
  { username     :: Validatable f Text (Maybe Username.ValidationErrors)
  , emailAddress :: Validatable f Text (First EmailAddress.ValidationError)
  , password     :: Validatable f Text (Maybe Password.ValidationErrors)
  }
  deriving stock Generic

type RawCreatable = RawCreatableV Identity

deriving stock instance Show RawCreatable
deriving stock instance Eq RawCreatable
deriving anyclass instance FromJSON RawCreatable
deriving anyclass instance ToJSON RawCreatable

type RawCreatableValidationErrors = RawCreatableV Validate

deriving via (GenericSemigroup RawCreatableValidationErrors) instance Semigroup RawCreatableValidationErrors
deriving via (GenericMonoid RawCreatableValidationErrors) instance Monoid RawCreatableValidationErrors

data ParsedCreatable = ParsedCreatable
  { username     :: Username
  , emailAddress :: EmailAddress
  , password     :: Password
  }
  deriving stock (Generic, Show, Eq)

parseRawCreatableUser :: RawCreatable
                      -> Validation
                           RawCreatableValidationErrors
                           ParsedCreatable
parseRawCreatableUser RawCreatable {..} =
  let validateUsername = Username.mk username |> first \e ->
        mempty { username = Just e } :: RawCreatableValidationErrors
      validateEmailAddress = EmailAddress.mk emailAddress |> maybeToSuccess
        (mempty { emailAddress = coerce $ Just EmailAddress.IsInvalid } :: RawCreatableValidationErrors
        )
      validatePassword =
        password |> encodeUtf8 |> Password.mk |> first \e ->
          mempty { password = Just e } :: RawCreatableValidationErrors
  in  ParsedCreatable
        <$> validateUsername
        <*> validateEmailAddress
        <*> validatePassword
