module Cascade.Api.Network.TestClient
  ( api
  , interpret
  ) where

import           Cascade.Api.Network.Anatomy    ( Routes )
import qualified Cascade.Api.Network.Anatomy.Api
                                               as Api
import           Control.Lens                   ( (^.) )
import           Control.Monad.Free
import qualified Network.HTTP.Client           as Http
import           Servant.API.Generic            ( fromServant )
import           Servant.Client.Core
import           Servant.Client.Free            ( ClientF(..) )
import           Servant.Client.Generic         ( AsClientT
                                                , genericClient
                                                )
import qualified Servant.Client.Internal.HttpClient
                                               as Http
                                                ( clientResponseToResponse
                                                , defaultMakeClientRequest
                                                )

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
