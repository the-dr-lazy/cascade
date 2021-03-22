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

{-# LANGUAGE UndecidableInstances #-}
module Cascade.Api.Data.Authentication (Credential(..), parseRawCredential) where

import qualified Cascade.Api.Data.Aeson.RecordErrorFormat
                                                    as Aeson
import qualified Cascade.Api.Data.ByteString.Password
                                                    as Password
import qualified Cascade.Api.Data.Text.Username     as Username
import qualified Cascade.Api.Data.User              as User
import           Cascade.Data.Validation             ( Validate
                                                     , Validation
                                                     )
import qualified Cascade.Data.Validation            as Validation
import           Data.Aeson                          ( FromJSON
                                                     , ToJSON
                                                     )

-- brittany-disable-next-binding
data Credential (p :: Validation.Phase) = Credential
  { username :: Validate p Text User.Username
  , password :: Validate p Text User.Password
  }
  deriving stock Generic

deriving stock instance Show (Credential 'Validation.Raw)
deriving via Aeson.RecordErrorFormat (Credential 'Validation.Raw) instance ToJSON (Credential 'Validation.Raw)
deriving via Aeson.RecordErrorFormat (Credential 'Validation.Raw) instance FromJSON (Credential 'Validation.Raw)

parseRawCredential :: Credential 'Validation.Raw -> Validation () (Credential 'Validation.Parsed)
parseRawCredential = first mempty . Validation.parseRecord Credential { username = Username.mk, password = Password.mk }
