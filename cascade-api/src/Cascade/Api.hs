{-|
Module      : Cascade.Api
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api
    ( Config (..)
    , main
    ) where

import qualified Cascade.Api.Effect.Database         as Database
import qualified Cascade.Api.Effect.Database.Project as Database.Project
import qualified Cascade.Api.Effect.Database.Task    as Database.Task
import qualified Cascade.Api.Effect.Database.User    as Database.User
import qualified Cascade.Api.Effect.Scrypt           as Scrypt
import qualified Cascade.Api.Effect.Time             as Time
import           Cascade.Api.Network.Wai.Application
import           Cascade.Api.Network.Wai.Log         (logMiddleware)
import           Cascade.Api.Orphans                 ()
import qualified Cascade.Logger                      as Logger
import qualified Cascade.Logger.Message              as Logger.Message
import           Colog                               (LogAction, usingLoggerT)
import qualified Database.PostgreSQL.Simple          as Postgres
import qualified Network.Wai.Handler.Warp            as Warp
import           Polysemy                            (runFinal)
import           Polysemy.Error                      (errorToIOFinal)
import           Polysemy.Final                      (embedToFinal)
import qualified Servant

data Config = Config { port                   :: Word16
                     , withDatabaseConnection :: forall a. (Postgres.Connection -> IO a) -> IO a
                     , logAction              :: LogAction IO Logger.Message.Minimal
                     }

main :: Config -> IO ()
main config@Config {..} = do
  let settings = Warp.defaultSettings |> Warp.setPort (fromIntegral port) |> Warp.setBeforeMainLoop (beforeMainLoopHook config)

  Warp.runSettings settings . logMiddleware logAction <| application
    ( Servant.Handler
    . ExceptT
    . runFinal
    . errorToIOFinal
    . embedToFinal
    . Database.postgresToFinal withDatabaseConnection
    . Scrypt.run
    . Time.run
    . Database.Project.run
    . Database.User.run
    . Database.Task.run
    )

beforeMainLoopHook :: Config -> IO ()
beforeMainLoopHook Config { logAction, port } = usingLoggerT logAction <| Logger.info ("Started listening on 127.0.0.1:" <> show port <> ".")
