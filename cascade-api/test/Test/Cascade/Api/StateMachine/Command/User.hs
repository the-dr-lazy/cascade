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

module Test.Cascade.Api.StateMachine.Command.User (commands) where

import qualified Cascade.Api.Data.User              as User
import           Cascade.Api.Hedgehog.Gen.Prelude
import qualified Cascade.Api.Hedgehog.Gen.Text      as Gen
import qualified Cascade.Api.Network.TestClient.Api.Users
                                                    as Cascade.Api.Users
import           Cascade.Api.Test.Prelude            ( )
import qualified Cascade.Data.Char                  as Char
import           Control.Lens                        ( (?~)
                                                     , (^.)
                                                     , _2
                                                     , at
                                                     , to
                                                     , view
                                                     )
import qualified Data.Map                           as Map
import qualified Data.Text                          as Text
import           Hedgehog
import qualified Hedgehog.Gen                       as Gen
import           Test.Cascade.Api.StateMachine.Model ( Model )

commands :: MonadGen g => GenBase g ~ Identity => MonadIO m => MonadTest m => [Command g m Model]
commands = [createNotExisting, createExisting]

-- brittany-disable-next-binding
data CreateNotExisting (v :: Type -> Type) = CreateNotExisting
  { validity :: Validity
  , creatable :: User.RawCreatable
  }
  deriving stock (Generic, Show)

instance HTraversable CreateNotExisting where
    htraverse _ CreateNotExisting {..} = pure $ CreateNotExisting { .. }

createNotExisting :: forall g m . MonadGen g => GenBase g ~ Identity => MonadIO m => MonadTest m => Command g m Model
createNotExisting =
    let
        generator :: Model Symbolic -> Maybe (g (CreateNotExisting Symbolic))
        generator model = Just do
            let usernames      = model ^. #user . #byUsername
                emailAddresses = model ^. #user . #byEmailAddress
            (usernameValidity    , username    ) <- Gen.filter (flip Map.notMember usernames . view _2) Gen.usernameWithValidity
            (emailAddressValidity, emailAddress) <- Gen.filter (flip Map.notMember emailAddresses . view _2) Gen.emailAddressWithValidity
            (passwordValidity    , password    ) <- Gen.passwordWithValidity
            let validity  = [usernameValidity, emailAddressValidity, passwordValidity] |> all (== Valid) |> bool Invalid Valid
                creatable = User.RawCreatable { .. }

            pure $ CreateNotExisting { .. }

        coverage :: CreateNotExisting Concrete -> m ()
        coverage (CreateNotExisting Valid   _        ) = pure ()
        coverage (CreateNotExisting Invalid creatable) = do
            let isUsernameTooShort    = creatable ^. #username . to Text.length < 8
                isUsernameTooLong     = creatable ^. #username . to Text.length > 20
                isUsernameInvalid     = creatable ^. #username |> Text.all Char.isAlphaNumUnderscore |> not
                isEmailAddressInvalid = creatable ^. #emailAddress |> Text.any (== '@') |> not
                isPasswordTooShort    = creatable ^. #password . to Text.length < 8
            cover 5 "too short username"    isUsernameTooShort
            cover 5 "too long username"     isUsernameTooLong
            cover 5 "invalid username"      isUsernameInvalid
            cover 5 "invalid email address" isEmailAddressInvalid
            cover 5 "too short password"    isPasswordTooShort

        execute :: CreateNotExisting Concrete -> m Cascade.Api.Users.CreateResponse
        execute input@(CreateNotExisting _ creatable) = do
            label "[User/Create Not Existing]"
            coverage input
            evalIO $ Cascade.Api.Users.create creatable

        require :: Model Symbolic -> CreateNotExisting Symbolic -> Bool
        require model (CreateNotExisting _ creatable) = doesNotUsernameExist
            where doesNotUsernameExist = Map.notMember (creatable ^. #username) (model ^. #user . #byUsername)

        update :: Model v -> CreateNotExisting v -> Var output v -> Model v
        update model (CreateNotExisting Invalid _) _ = model
        update model (CreateNotExisting Valid creatable) _ =
            model |> (#user . #byUsername . at username ?~ creatable) |> (#user . #byEmailAddress . at emailAddress ?~ creatable)
          where
            username     = creatable ^. #username
            emailAddress = creatable ^. #emailAddress

        ensure :: Model Concrete -> Model Concrete -> CreateNotExisting Concrete -> Cascade.Api.Users.CreateResponse -> Test ()
        ensure _ _ input response = do
            footnoteShow response
            let statusCode = response ^. #responseStatusCode . #statusCode
            case input of
                CreateNotExisting Valid   _ -> statusCode === 201
                CreateNotExisting Invalid _ -> statusCode === 422
    in
        Command generator execute [Require require, Update update, Ensure ensure]

-- brittany-disable-next-binding
newtype CreateExisting (v :: Type -> Type) = CreateExisting
  { creatable :: User.RawCreatable }
  deriving stock Show

instance HTraversable CreateExisting where
    htraverse _ input = pure $ coerce input

createExisting :: forall g m . MonadGen g => MonadIO m => MonadTest m => Command g m Model
createExisting =
    let generator :: Model Symbolic -> Maybe (g (CreateExisting Symbolic))
        generator model | null usernames || null emailAddresses = Nothing
                        | otherwise                             = creatable |> fmap CreateExisting |> Just
              where
                usernames      = model ^. #user . #byUsername . to Map.keys
                emailAddresses = model ^. #user . #byEmailAddress . to Map.keys
                creatable      = User.RawCreatable <$> Gen.element usernames <*> Gen.element emailAddresses <*> Gen.password Valid

        execute :: CreateExisting Concrete -> m Cascade.Api.Users.CreateResponse
        execute (CreateExisting creatable) = do
            label "[User/Create Existing]"
            evalIO $ Cascade.Api.Users.create creatable

        require :: Model Symbolic -> CreateExisting Symbolic -> Bool
        require model (CreateExisting creatable) = doesUsernameExist && doesEmailAddressExist
              where
                doesUsernameExist     = Map.member (creatable ^. #username) (model ^. #user . #byUsername)
                doesEmailAddressExist = Map.member (creatable ^. #emailAddress) (model ^. #user . #byEmailAddress)

        ensure :: Model Concrete -> Model Concrete -> CreateExisting Concrete -> Cascade.Api.Users.CreateResponse -> Test ()
        ensure _ _ _ response = do
            footnoteShow response
            response ^. #responseStatusCode . #statusCode === 409
    in  Command generator execute [Require require, Ensure ensure]
