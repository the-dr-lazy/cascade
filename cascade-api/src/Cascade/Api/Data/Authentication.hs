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

module Cascade.Api.Data.Authentication (RawCredential(..), ParsedCredential, parseRawCredential) where

import qualified Cascade.Api.Data.ByteString.Password
                                                    as Password
import qualified Cascade.Api.Data.Text.Username     as Username
import qualified Cascade.Api.Data.User              as User
import           Data.Aeson                          ( FromJSON
                                                     , ToJSON
                                                     )
import           Validation

data RawCredential = RawCredential
    { username :: Text
    , password :: Text
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromJSON, ToJSON)

data ParsedCredential = ParsedCredential
    { username :: User.Username
    , password :: User.Password
    }
    deriving stock (Generic, Show, Eq)

parseRawCredential :: RawCredential -> Validation () ParsedCredential
parseRawCredential RawCredential {..} =
    let validateUsername = Username.mk username |> first mempty
        validatePassword = Password.mk password |> first mempty
    in  ParsedCredential <$> validateUsername <*> validatePassword
