{-|
Module      : Cascade.Api.Network.Server.Api.Authentication
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.Server.Api.Authentication (server) where

import qualified Cascade.Api.Data.Authentication    as Authentication
import           Cascade.Api.Data.Authentication     ( parseRawCredential )
import qualified Cascade.Api.Data.Jwt               as Jwt
import qualified Cascade.Api.Effect.Database.User   as Database
                                                     ( UserL )
import qualified Cascade.Api.Effect.Database.User   as Database.User
import qualified Cascade.Api.Effect.Scrypt          as Scrypt
import           Cascade.Api.Network.Anatomy.Api.Authentication
import           Cascade.Api.Servant.Authentication  ( headerAndPayloadCookieName
                                                     , signatureCookieName
                                                     )
import           Control.Lens                        ( (^.)
                                                     , _Wrapped'
                                                     )
import           Polysemy                            ( Members
                                                     , Sem
                                                     )
import           Polysemy.Error                      ( Error
                                                     , throw
                                                     )
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic              ( AsServerT
                                                     , genericServerT
                                                     )
import           Validation                          ( validation )
import           Web.Cookie
import qualified Web.Cookie                         as Cookie

handleLogin :: Members '[Database.UserL , Error ServerError] r => Authentication.RawCredential -> Sem r LoginResponse
handleLogin = validation (const $ throw err422) go . parseRawCredential
 where
  go :: Members '[Database.UserL , Error ServerError] r => Authentication.ParsedCredential -> Sem r LoginResponse
  go credential = Database.User.findByUsername (credential ^. #username) >>= maybe
    (throw err401)
    \user -> do
      let doesPasswordsMatch = Scrypt.verifyPassword (credential ^. #password) (user ^. #encryptedPassword . _Wrapped')
      unless doesPasswordsMatch $ throw err401
      pure $ mkLoginResponse (Jwt.mk $ user ^. #id . _Wrapped')

server :: Members '[Database.UserL , Error ServerError] r => ToServant Routes (AsServerT (Sem r))
server = genericServerT Routes { login = handleLogin }

mkLoginResponse :: (ByteString, ByteString) -> LoginResponse
mkLoginResponse (headerAndPayload, sig) = NoContent |> addHeader setHeaderAndPayloadCookie |> addHeader setSignatureCookie
 where
  setHeaderAndPayloadCookie = mkSetHeaderAndPayloadCookie headerAndPayload
  setSignatureCookie        = mkSetSignatureCookie sig

mkSetHeaderAndPayloadCookie :: ByteString -> SetCookie
mkSetHeaderAndPayloadCookie value = defaultSetCookie { setCookieName     = headerAndPayloadCookieName
                                                     , setCookieValue    = value
                                                     , setCookieSecure   = True
                                                     , setCookieSameSite = Just Cookie.sameSiteStrict
                                                     }


mkSetSignatureCookie :: ByteString -> SetCookie
mkSetSignatureCookie value = defaultSetCookie { setCookieName     = signatureCookieName
                                              , setCookieValue    = value
                                              , setCookieHttpOnly = True
                                              , setCookieSecure   = True
                                              , setCookieSameSite = Just Cookie.sameSiteStrict
                                              }
