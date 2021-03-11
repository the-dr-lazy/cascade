{-|
Module      : Test.Cascade.Api.StateMachine.Command.User
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Test.Cascade.Api.StateMachine.Command.User
  ( commands
  ) where

import           Hedgehog
import           Test.Cascade.Api.StateMachine.Command.User.CreateExisting
import           Test.Cascade.Api.StateMachine.Command.User.CreateNotExisting
import           Test.Cascade.Api.StateMachine.Model
                                                ( Model )

commands :: MonadGen g => MonadIO m => MonadTest m => [Command g m Model]
commands = [createNotExisting, createExisting]
