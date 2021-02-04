{-|
Module      : Cascade.Api.Servant.Authentication
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Servant.Authentication
  ( Auth
  , handleAuthentication
  ) where

import qualified Cascade.Api.Data.Jwt          as Jwt
import qualified Cascade.Api.Data.Session      as Session
import           Cascade.Api.Data.Session       ( Session )
import qualified Cascade.Data.ByteString       as W8
import qualified Data.ByteString               as W8
import qualified Data.List                     as List
import           Network.HTTP.Types             ( hAuthorization
                                                , hCookie
                                                )
import qualified Network.Wai                   as Wai
import           Network.Wai                    ( requestHeaders )
import           Servant                        ( Handler
                                                , throwError
                                                )
import           Servant.API.Experimental.Auth
import           Servant.Server                 ( err401 )
import           Servant.Server.Experimental.Auth
import           Web.Cookie                     ( parseCookies )

type Auth = AuthProtect "JWT"

type instance AuthServerData Auth = Session

handleAuthentication :: Wai.Request -> Handler Session
handleAuthentication request = maybe
  (pure Session.Anonymous)
  (   maybe (throwError err401)
            (pure . Session.Authenticated . Jwt.getPrivateClaims)
  <=< (liftIO . Jwt.decode)
  )
  token
 where
  sig =
    (request |> requestHeaders |> List.lookup hAuthorization)
      >>= fmap W8.trim
      .   W8.stripPrefix "Bearer"
  headerAndPayload =
    (request |> requestHeaders |> List.lookup hCookie)
      >>= List.lookup "Signature"
      .   parseCookies
  token = Jwt.reassociate <$> headerAndPayload <*> sig
