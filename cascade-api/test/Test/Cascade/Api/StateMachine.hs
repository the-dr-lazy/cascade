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

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Test.Cascade.Api.StateMachine (tests) where

import qualified Cascade.Api
import           Control.Concurrent.Async.Lifted

import qualified Cascade.Api.Test.Resource          as Resource
import           Control.Monad.Managed
import           Control.Monad.Trans.Control
import           Data.Pool                           ( Pool )
import qualified Data.Pool                          as Pool
import qualified Database.PostgreSQL.Simple         as Postgres
import qualified Database.Postgres.Temp             as TempPostgres
import           Hedgehog
import qualified Hedgehog.Gen                       as Gen
import qualified Hedgehog.Range                     as Range
import qualified Network.Socket.Wait                as Socket
import qualified Test.Cascade.Api.StateMachine.Command.Authentication
                                                    as Command.Authentication
import qualified Test.Cascade.Api.StateMachine.Command.Project
                                                    as Command.Project
import qualified Test.Cascade.Api.StateMachine.Command.Task
                                                    as Command.Task
import qualified Test.Cascade.Api.StateMachine.Command.User
                                                    as Command.User
import           Test.Cascade.Api.StateMachine.Model
import           Test.Tasty
import           Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Test.Cascade.Api.StateMachine" [Resource.withMigratedDatabaseConfig (testProperty "Sequential" . prop_sequential)]

prop_sequential :: IO TempPostgres.Config -> Property
prop_sequential getMigratedDatabase = withTests 300 . withDiscards 500 . property $ do
  db      <- evalIO getMigratedDatabase
  actions <- forAll $ Gen.sequential (Range.exponential 1 144) initialModel commands

  control \runInBase -> flip with pure $ do
    pool <- Resource.withPostgresConnectionPool db
    liftIO $ withAsync
      (Cascade.Api.main $ Pool.withResource pool)
      \_ -> do
        Socket.wait "127.0.0.1" 3141
        runInBase $ executeSequential initialModel actions

commands :: _ => [Command g m Model]
commands = mconcat [Command.User.commands, Command.Authentication.commands, Command.Project.commands, Command.Task.commands]
