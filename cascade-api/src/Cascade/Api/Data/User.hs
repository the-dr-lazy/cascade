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

{-# LANGUAGE UndecidableInstances #-}
module Cascade.Api.Data.User
    ( Creatable (..)
    , EmailAddress
    , Id
    , Password
    , Readable (..)
    , User
    , Username
    , parseRawCreatable
    ) where

import qualified Cascade.Api.Data.Aeson.RecordErrorFormat as Aeson
import           Cascade.Api.Data.ByteString.Password     ( Password )
import qualified Cascade.Api.Data.ByteString.Password     as Password
import qualified Cascade.Api.Data.Id                      as Data
import           Cascade.Api.Data.Text.EmailAddress       ( EmailAddress )
import qualified Cascade.Api.Data.Text.EmailAddress       as EmailAddress
import           Cascade.Api.Data.Text.Username           ( Username )
import qualified Cascade.Api.Data.Text.Username           as Username
import           Cascade.Data.Validation                  ( Validate, Validation )
import qualified Cascade.Data.Validation                  as Validation
import           Data.Aeson                               ( FromJSON, ToJSON )
import           Data.Generics.Labels                     ()

data User

type Id = Data.Id User

data Readable = Readable { id           :: Id
                         , username     :: Username
                         , emailAddress :: EmailAddress
                         }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data Creatable p = Creatable { username     :: Validate p Text Username
                             , emailAddress :: Validate p Text EmailAddress
                             , password     :: Validate p Text Password
                             }
  deriving stock (Generic)

deriving stock instance Show (Creatable 'Validation.Raw)
deriving anyclass instance ToJSON   (Creatable 'Validation.Raw)
deriving anyclass instance FromJSON (Creatable 'Validation.Raw)

deriving stock instance Show (Creatable 'Validation.Error)
deriving via Aeson.RecordErrorFormat (Creatable 'Validation.Error) instance ToJSON (Creatable 'Validation.Error)
deriving via Aeson.RecordErrorFormat (Creatable 'Validation.Error) instance FromJSON (Creatable 'Validation.Error)

parseRawCreatable :: Creatable 'Validation.Raw -> Validation (Creatable 'Validation.Error) (Creatable 'Validation.Parsed)
parseRawCreatable = Validation.parseRecord Creatable { username = Username.mk, emailAddress = EmailAddress.parse, password = Password.mk }
