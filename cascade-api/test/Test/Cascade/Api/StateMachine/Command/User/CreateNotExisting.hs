{-|
Module      : Test.Cascade.Api.StateMachine.Command.User.CreateNotExisting
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Test.Cascade.Api.StateMachine.Command.User.CreateNotExisting (createNotExisting) where

import qualified Cascade.Api.Data.User              as User
import           Cascade.Api.Hedgehog.Gen.Prelude
import qualified Cascade.Api.Hedgehog.Gen.Text      as Gen
import qualified Cascade.Api.Network.TestClient.Api.Users
                                                    as Cascade.Api.Users
import           Cascade.Api.Test.Prelude            ( )
import qualified Cascade.Data.Char                  as Char
import           Control.Lens                        ( (?~)
                                                     , (^.)
                                                     , at
                                                     , hasn't
                                                     , ix
                                                     , to
                                                     )
import qualified Data.Text                          as Text
import           Hedgehog
import qualified Hedgehog.Gen                       as Gen
import           Test.Cascade.Api.StateMachine.Model ( Model )
-- brittany-disable-next-binding
data CreateNotExisting (v :: Type -> Type) = CreateNotExisting
  { validity  :: Validity
  , creatable :: User.RawCreatable
  }
  deriving stock (Generic, Show)

instance HTraversable CreateNotExisting where
  htraverse _ = pure . coerce

createNotExisting :: MonadGen g => MonadIO m => MonadTest m => Command g m Model
createNotExisting = Command generator execute [Update update, Ensure ensure]

generator :: MonadGen g => Model Symbolic -> Maybe (g (CreateNotExisting Symbolic))
generator model = Just do
  (usernameValidity    , username    ) <- Gen.usernameWithValidity
  (emailAddressValidity, emailAddress) <- Gen.emailAddressWithValidity
  (passwordValidity    , password    ) <- Gen.passwordWithValidity
  let validity  = fold [usernameValidity, emailAddressValidity, passwordValidity]
      creatable = User.RawCreatable { .. }

  let isUsernameUnique     = model |> hasn't (#user . #byUsername . ix username)
      isEmailAddressUnique = model |> hasn't (#user . #byEmailAddress . ix emailAddress)

  unless (isUsernameUnique && isEmailAddressUnique) Gen.discard

  pure $ CreateNotExisting { .. }

coverage :: MonadTest m => CreateNotExisting Concrete -> m ()
coverage (CreateNotExisting Valid   _        ) = pure ()
coverage (CreateNotExisting Invalid creatable) = do
  cover 5 "too short username"    isUsernameTooShort
  cover 5 "too long username"     isUsernameTooLong
  cover 5 "invalid username"      isUsernameInvalid
  cover 5 "invalid email address" isEmailAddressInvalid
  cover 5 "too short password"    isPasswordTooShort
 where
  User.RawCreatable { username, emailAddress, password } = creatable
  isUsernameTooShort    = Text.length username < 8
  isUsernameTooLong     = Text.length username > 20
  isUsernameInvalid     = not . Text.all Char.isAlphaNumUnderscore <| username
  isEmailAddressInvalid = not . Text.any (== '@') <| emailAddress
  isPasswordTooShort    = Text.length password < 8

execute :: MonadIO m => MonadTest m => CreateNotExisting Concrete -> m Cascade.Api.Users.CreateResponse
execute input@(CreateNotExisting _ creatable) = do
  label "[User/Create Not Existing]"
  coverage input
  evalIO . Cascade.Api.Users.create <| creatable

update :: Model v -> CreateNotExisting v -> Var output v -> Model v
update model CreateNotExisting { validity = Invalid } _ = model
update model CreateNotExisting { validity = Valid, creatable } _ =
  model |> (#user . #byUsername . at username ?~ creatable) |> (#user . #byEmailAddress . at emailAddress ?~ creatable)
  where User.RawCreatable { username, emailAddress } = creatable

ensure :: Model Concrete -> Model Concrete -> CreateNotExisting Concrete -> Cascade.Api.Users.CreateResponse -> Test ()
ensure _ _ input response = do
  footnoteShow response
  let statusCode = response ^. #responseStatusCode . #statusCode
  case input of
    CreateNotExisting Valid   _ -> statusCode === 201
    CreateNotExisting Invalid _ -> statusCode === 422
