{-|
Module      : Test.Cascade.Api.StateMachine
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Test.Cascade.Api.StateMachine
  ( tests
  ) where

import qualified Cascade.Api
import           Control.Concurrent.Async.Lifted

import qualified Cascade.Api.Test.Resource     as Resource
import           Control.Monad.Managed
import           Control.Monad.Trans.Control
import           Data.Pool                      ( Pool )
import qualified Database.PostgreSQL.Simple    as Postgres
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import qualified Network.Socket.Wait           as Socket
import qualified Test.Cascade.Api.StateMachine.Command.Project
                                               as Command.Project
import           Test.Cascade.Api.StateMachine.Model
import           Test.Tasty
import           Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup
  "Test.Cascade.Api.StateMachine"
  [ Resource.withTemporaryPostgresConnectionPool
      (testProperty "Sequential" . prop_sequential)
  ]

prop_sequential :: IO (Pool Postgres.Connection) -> Property
prop_sequential getPool = withTests 100 . property $ do
  pool    <- evalIO getPool
  actions <- forAll $ Gen.sequential (Range.linear 1 100) initialModel commands

  control \runInBase -> flip with pure $ do
    connection <- Resource.withPostgresConnectionInAbortionBracket pool
    liftIO $ withAsync
      (Cascade.Api.main \f -> f connection)
      \_ -> do
        Socket.wait "127.0.0.1" 3141
        runInBase $ executeSequential initialModel actions

commands :: MonadGen g => MonadIO m => MonadTest m => [Command g m Model]
commands = Command.Project.commands
