{-|
Module      : Test.Cascade.Api.StateMachine.Command.Authentication.InvalidLogin
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Test.Cascade.Api.StateMachine.Command.Authentication.InvalidLogin
    ( invalidLogin
    ) where

import qualified Cascade.Api.Data.Authentication                            as Authentication
import qualified Cascade.Api.Hedgehog.Gen                                   as Gen
import           Cascade.Api.Hedgehog.Gen.Prelude
import qualified Cascade.Api.Hedgehog.Gen.Text                              as Gen
import qualified Cascade.Api.Network.TestClient.Api.Authentication          as Cascade.Api.Authentication
import           Cascade.Api.Test.Prelude                                   ()
import qualified Cascade.Data.Char                                          as Char
import qualified Cascade.Data.Validation                                    as Validation
import           Control.Lens                                               ((^.))
import qualified Data.Text                                                  as Text
import           Hedgehog
import           Test.Cascade.Api.StateMachine.Command.Authentication.Types
import           Test.Cascade.Api.StateMachine.Model                        (Model)

invalidLogin :: MonadGen g => MonadFail g => MonadTest m => MonadIO m => Command g m Model
invalidLogin = Command generator execute []

generator :: MonadGen g => MonadFail g => Model Symbolic -> Maybe (g (Login Symbolic))
generator _ = Just do
  [usernameValidity, passwordValidity] <- Gen.replicateAtLeastOne Invalid 2
  username                             <- Gen.username usernameValidity
  password                             <- Gen.password passwordValidity

  pure . Login <| Authentication.Credential { .. }

coverage :: MonadTest m => Authentication.Credential 'Validation.Raw -> m ()
coverage Authentication.Credential { username, password } = do
  cover 5 "too short username" isUsernameTooShort
  cover 5 "too long username"  isUsernameTooLong
  cover 5 "invalid username"   isUsernameInvalid
  cover 5 "too short password" isPasswordTooShort
 where
  isUsernameTooShort = Text.length username < 8
  isUsernameTooLong  = Text.length username > 20
  isUsernameInvalid  = not . Text.all Char.isAlphaNumUnderscore <| username
  isPasswordTooShort = Text.length password < 8

execute :: MonadIO m => MonadTest m => Login Concrete -> m ()
execute (Login credential) = do
  label "[Authentication/Invalid Login]"
  coverage credential
  ensure =<< (evalIO . Cascade.Api.Authentication.login <| credential)

ensure :: MonadTest m => Cascade.Api.Authentication.LoginResponse -> m ()
ensure response = response ^. #responseStatusCode . #statusCode === 422
