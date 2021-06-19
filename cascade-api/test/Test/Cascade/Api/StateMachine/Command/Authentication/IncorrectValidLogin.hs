{-|
Module      : Test.Cascade.Api.StateMachine.Command.Authentication.IncorrectValidLogin
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Test.Cascade.Api.StateMachine.Command.Authentication.IncorrectValidLogin
    ( incorrectValidLogin
    ) where

import qualified Cascade.Api.Data.Authentication                            as Authentication
import           Cascade.Api.Hedgehog.Gen.Prelude
import qualified Cascade.Api.Hedgehog.Gen.Text                              as Gen
import qualified Cascade.Api.Network.TestClient.Api.Authentication          as Cascade.Api.Authentication
import           Cascade.Api.Test.Prelude                                   ()
import           Cascade.Data.Foldable                                      ( defaulting )
import           Control.Lens
    ( asIndex, ifolded, ix, (^.), (^..), (^?) )
import           Hedgehog
import qualified Hedgehog.Gen                                               as Gen
import           Test.Cascade.Api.StateMachine.Command.Authentication.Types
import           Test.Cascade.Api.StateMachine.Model                        ( Model )

incorrectValidLogin :: MonadGen g => MonadTest m => MonadIO m => Command g m Model
incorrectValidLogin = Command generator execute []

generator :: MonadGen g => Model Symbolic -> Maybe (g (Login Symbolic))
generator model = Just do
  let usernames = defaulting Nothing Just <| model ^.. #user . #byUsername . ifolded . asIndex
  username <- Gen.choice . catMaybes <| [fmap Gen.element usernames, Just <| Gen.username Valid]
  password <- Gen.password Valid |> Gen.filterT \password -> Just password /= model ^? #user . #byUsername . ix username . #password

  pure . Login <| Authentication.Credential { .. }

execute :: MonadIO m => MonadTest m => Login Concrete -> m ()
execute (Login credential) = do
  label "[Authentication/Incorrect Valid Login]"
  ensure =<< (evalIO . Cascade.Api.Authentication.login <| credential)

ensure :: MonadTest m => Cascade.Api.Authentication.LoginResponse -> m ()
ensure response = response ^. #responseStatusCode . #statusCode === 401
