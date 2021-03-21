{-|
Module      : Cascade.Api.Network.TestClient.Api.Authentication
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.TestClient.Api.Authentication (login, LoginResponse) where

import qualified Cascade.Api.Data.Authentication    as Authentication
import qualified Cascade.Api.Network.Anatomy.Api.Authentication
                                                    as Api.Authentication
import           Cascade.Api.Network.TestClient      ( interpret )
import qualified Cascade.Api.Network.TestClient.Api as Client.Api
import           Control.Lens                        ( (^.) )
import           Servant.Client                      ( ResponseF )

type LoginResponse = ResponseF Api.Authentication.LoginResponse

login :: Authentication.RawCredential -> IO LoginResponse
login = interpret . go where go = Client.Api.authentication ^. #login
