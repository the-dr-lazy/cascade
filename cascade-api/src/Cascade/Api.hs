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

module Cascade.Api (main) where

import qualified Cascade.Api.Effect.Database        as Database
import qualified Cascade.Api.Effect.Database.Project
                                                    as Database.Project
import qualified Cascade.Api.Effect.Database.User   as Database.User
import qualified Cascade.Api.Effect.Scrypt          as Scrypt
import           Cascade.Api.Network.Wai.Application
import           Cascade.Api.Orphans                 ( )
import qualified Database.PostgreSQL.Simple         as Postgres
import qualified Network.Wai.Handler.Warp           as Warp
import           Polysemy                            ( runFinal )
import           Polysemy.Error                      ( errorToIOFinal )
import           Polysemy.Final                      ( embedToFinal )
import qualified Servant

main :: (forall a . (Postgres.Connection -> IO a) -> IO a) -> IO ()
main withDatabaseConnection = do
    Warp.run 3141 $ application
        ( Servant.Handler
        . ExceptT
        . runFinal
        . errorToIOFinal
        . embedToFinal
        . Scrypt.run
        . Database.runPostgres withDatabaseConnection
        . Database.Project.run
        . Database.User.run
        )
