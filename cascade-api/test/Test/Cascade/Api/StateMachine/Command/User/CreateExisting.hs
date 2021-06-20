{-|
Module      : Test.Cascade.Api.StateMachine.Command.User.CreateExisting
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Test.Cascade.Api.StateMachine.Command.User.CreateExisting
    ( createExisting
    ) where

import qualified Cascade.Api.Data.User                    as User
import           Cascade.Api.Hedgehog.Gen.Prelude
import qualified Cascade.Api.Hedgehog.Gen.Text            as Gen
import qualified Cascade.Api.Network.TestClient.Api.Users as Cascade.Api.Users
import           Cascade.Api.Test.Prelude                 ()
import qualified Cascade.Data.Validation                  as Validation
import           Control.Lens                             ( has, ix, to, (^.) )
import qualified Data.Map                                 as Map
import           Hedgehog
import qualified Hedgehog.Gen                             as Gen
import           Test.Cascade.Api.StateMachine.Model      ( Model )

createExisting :: MonadGen g => MonadIO m => MonadTest m => Command g m Model
createExisting = Command generator execute [Require require, Ensure ensure]


newtype CreateExisting (v :: Type -> Type)
  = CreateExisting { creatable :: User.Creatable Validation.Raw }
  deriving stock (Show)

instance HTraversable CreateExisting where
  htraverse _ input = pure $ coerce input

generator :: MonadGen g => Model Symbolic -> Maybe (g (CreateExisting Symbolic))
generator model | null usernames || null emailAddresses = Nothing
                | otherwise                             = creatable |> fmap CreateExisting |> Just
 where
  usernames      = model ^. #user . #byUsername . to Map.keys
  emailAddresses = model ^. #user . #byEmailAddress . to Map.keys
  creatable      = User.Creatable <$> Gen.element usernames <*> Gen.element emailAddresses <*> Gen.password Valid

require :: Model Symbolic -> CreateExisting Symbolic -> Bool
require model (CreateExisting User.Creatable { username, emailAddress }) = doesUsernameExist || doesEmailAddressExist
 where
  doesUsernameExist     = model |> has (#user . #byUsername . ix username)
  doesEmailAddressExist = model |> has (#user . #byEmailAddress . ix emailAddress)

execute :: MonadIO m => MonadTest m => CreateExisting Concrete -> m Cascade.Api.Users.CreateResponse
execute (CreateExisting creatable) = do
  label "[User/Create Existing]"
  evalIO $ Cascade.Api.Users.create creatable

ensure :: Model Concrete -> Model Concrete -> CreateExisting Concrete -> Cascade.Api.Users.CreateResponse -> Test ()
ensure _ _ _ response = do
  footnoteShow response
  response ^. #responseStatusCode . #statusCode === 409
