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

import qualified Cascade.Api.Data.User              as User
import           Cascade.Data.Validation
import           Data.Aeson                          ( FromJSON
                                                     , ToJSON
                                                     )
import qualified Polysemy

-- brittany-disable-next-binding
data Credential (v :: Phase) = Credential
  { username :: Validate v User.Username
  , password :: Validate v User.Password
  }
  deriving stock (Generic)
  deriving Validatable via (Generically (Credential v))

deriving anyclass instance ToJSON (Credential 'Raw)
deriving anyclass instance FromJSON (Credential 'Raw)

parseRawCredential :: Credential 'Raw -> Validation () (Credential 'Parsed)
parseRawCredential = first mempty . Polysemy.run . validate @(Credential 'Raw)
