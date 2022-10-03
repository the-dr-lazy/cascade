{-|
Module      : Test.Cascade.Api.StateMachine.Command.Project.UpdateNotExistingById
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2022 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Test.Cascade.Api.StateMachine.Command.Project.UpdateNotExistingById
    ( updateNotExistingById
    ) where

import qualified Cascade.Api.Hedgehog.Gen.Api.Project                as Gen
import qualified Cascade.Api.Network.TestClient.Api.Projects         as Cascade.Api.Projects
import           Cascade.Api.Test.Prelude                            ()
import           Control.Lens                                        ( has, ix, (^.), (^@..) )
import           Hedgehog
import qualified Hedgehog.Gen                                        as Gen
import           Test.Cascade.Api.StateMachine.Command.Project.Types
import           Test.Cascade.Api.StateMachine.Model                 ( Model )
import qualified Test.Cascade.Api.StateMachine.Model.Lens            as Model.Lens

updateNotExistingById :: MonadGen g => MonadIO m => MonadTest m => Command g m Model
updateNotExistingById = Command generator execute [Require require, Ensure ensure]

generator :: MonadGen g => Model Symbolic -> Maybe (g (UpdateById Symbolic))
generator model = case usernameTokenProjectIdAList of
  [] -> Nothing
  _  -> Just do
    (username, token, id) <- Gen.element usernameTokenProjectIdAList
    updatable             <- Gen.project
    pure UpdateById { .. }
 where
  usernameTokenProjectIdAList = do
    (username, token) <- model ^@.. Model.Lens.indexTokenByUsername
    id                <- model ^. #project . #notExistingIds
    pure (username, token, id)

require :: Model Symbolic -> UpdateById Symbolic -> Bool
require model UpdateById { username } = model |> has (#authToken . #byUsername . ix username)

execute :: MonadIO m => MonadTest m => UpdateById Concrete -> m Cascade.Api.Projects.UpdateByIdResponse
execute UpdateById { token, id, updatable } = do
  label "[Project/Update Not Existing By ID]"
  evalIO $ Cascade.Api.Projects.updateById (concrete token) (concrete id) updatable

ensure :: Model Concrete -> Model Concrete -> UpdateById Concrete -> Cascade.Api.Projects.UpdateByIdResponse -> Test ()
ensure _ _ _ response = do
  footnoteShow response
  response ^. #responseStatusCode . #statusCode === 404
