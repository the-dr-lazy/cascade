{-|
Module      : Cascade.Api.Network.TestClient
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.TestClient
    ( AuthToken
    , api
    , authenticated
    , interpret
    ) where

import           Cascade.Api.Data.Jwt               ( JwtSections )
import           Cascade.Api.Network.Anatomy        ( Routes )
import qualified Cascade.Api.Network.Anatomy.Api    as Api
import           Cascade.Api.Servant.Authentication
import           Control.Lens                       ( (^.) )
import           Control.Monad.Free
import qualified Data.Binary.Builder                as Builder
import qualified Data.ByteString.Lazy               as LW8
import qualified Data.Sequence                      as Seq
import qualified Network.HTTP.Client                as Http
import           Network.HTTP.Types                 ( hCookie )
import           Servant.API.Generic                ( fromServant )
import           Servant.Client.Core
import           Servant.Client.Free                ( ClientF (..) )
import           Servant.Client.Generic             ( AsClientT, genericClient )
import qualified Servant.Client.Internal.HttpClient as Http
    ( clientResponseToResponse, defaultMakeClientRequest )
import           Web.Cookie                         ( renderCookies )

client :: Routes (AsClientT (Free ClientF))
client = genericClient

api :: Api.Routes (AsClientT (Free ClientF))
api = fromServant $ client ^. #api

interpret :: Free ClientF a -> IO (ResponseF a)
interpret x = case x of
  Pure _                          -> error "ERROR: got (Pure a)."
  Free (Throw clientError       ) -> error $ "ERROR: " <> show clientError
  Free (RunRequest request parse) -> do
    baseUrl <- parseBaseUrl "http://localhost:3141"
    manager <- Http.newManager Http.defaultManagerSettings
    let request' = Http.defaultMakeClientRequest baseUrl request
    response' <- Http.httpLbs request' manager
    let response = Http.clientResponseToResponse identity response'
    case parse response of
      Pure body                -> pure $ response $> body
      Free (Throw clientError) -> error $ "ERROR: " <> show clientError
      _                        -> error "ERROR: didn't got response."

type instance AuthClientData Auth = JwtSections

type AuthToken = AuthClientData Auth

authenticated :: AuthToken -> AuthenticatedRequest Auth
authenticated sections@(headerAndPayload, sig) = mkAuthenticatedRequest
  sections
  \_ request -> request { requestHeaders = request |> requestHeaders |> (Seq.|> (hCookie, cookie)) }
 where
  cookie =
    renderCookies [(headerAndPayloadCookieName, headerAndPayload), (signatureCookieName, sig)] |> Builder.toLazyByteString |> toStrict
