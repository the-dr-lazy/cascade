{-|
Module      : Test.Cascade.Api.StateMachine.Command.Authentication
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2022 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Test.Cascade.Api.StateMachine.Command.Authentication
    ( commands
    ) where

import           Hedgehog
import           Test.Cascade.Api.StateMachine.Command.Authentication.CorrectValidLogin
import           Test.Cascade.Api.StateMachine.Command.Authentication.IncorrectValidLogin
import           Test.Cascade.Api.StateMachine.Command.Authentication.InvalidLogin
import           Test.Cascade.Api.StateMachine.Model                                      ( Model )

commands :: MonadGen g => MonadTest m => MonadFail g => MonadIO m => [Command g m Model]
commands = [invalidLogin, incorrectValidLogin, correctValidLogin]
