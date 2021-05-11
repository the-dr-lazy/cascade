module Cascade.Api.Network.Wai.Log (logMiddleware) where

import           Cascade.Colog.Message               ( Message
                                                     , log
                                                     )
import           Colog                               ( LogAction
                                                     , Severity(..)
                                                     , usingLoggerT
                                                     )
import           Network.Wai                         ( Middleware
                                                     , Request
                                                     , rawPathInfo
                                                     , requestMethod
                                                     )

prettyPrintRequest :: Request -> Text
prettyPrintRequest req = "Request " <> show (requestMethod req) <> " " <> show (rawPathInfo req)

logMiddleware :: LogAction IO Message -> Middleware
logMiddleware logger app req respond = do
  usingLoggerT logger (log Info (prettyPrintRequest req))
  app req respond
