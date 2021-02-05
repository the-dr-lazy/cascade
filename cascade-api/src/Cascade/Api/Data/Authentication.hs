{-|
Module      : Cascade.Api.Data.Authentication
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Data.Authentication
  ( RawCredentialV(..)
  , RawCredential
  , RawCredentialValidationErrors
  , ParsedCredential
  , parseRawCredential
  ) where

import qualified Cascade.Api.Data.ByteString.Password
                                               as Password
import           Cascade.Api.Data.Prelude
import qualified Cascade.Api.Data.Text.Username
                                               as Username
import qualified Cascade.Api.Data.User         as User
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Monoid.Generic
import           Validation

-- brittany-disable-next-binding
data RawCredentialV (f :: Type -> Type) = RawCredential
  { username :: Validatable f Text (Maybe Username.ValidationErrors)
  , password :: Validatable f Text (Maybe Password.ValidationErrors)
  }
  deriving stock Generic

type RawCredential = RawCredentialV Identity

deriving stock instance Show RawCredential
deriving stock instance Eq RawCredential
deriving anyclass instance FromJSON RawCredential
deriving anyclass instance ToJSON RawCredential

type RawCredentialValidationErrors = RawCredentialV Validate

deriving stock instance Show RawCredentialValidationErrors
deriving anyclass instance ToJSON RawCredentialValidationErrors
deriving via (GenericSemigroup RawCredentialValidationErrors) instance Semigroup RawCredentialValidationErrors
deriving via (GenericMonoid RawCredentialValidationErrors) instance Monoid RawCredentialValidationErrors

data ParsedCredential = ParsedCredential
  { username :: User.Username
  , password :: User.Password
  }
  deriving stock (Generic, Show, Eq)

parseRawCredential :: RawCredential
                   -> Validation
                        RawCredentialValidationErrors
                        ParsedCredential
parseRawCredential RawCredential {..} =
  let validateUsername = Username.mk username |> first \e ->
        mempty { username = Just e } :: RawCredentialValidationErrors
      validatePassword =
        password |> encodeUtf8 |> Password.mk |> first \e ->
          mempty { password = Just e } :: RawCredentialValidationErrors
  in  ParsedCredential <$> validateUsername <*> validatePassword
