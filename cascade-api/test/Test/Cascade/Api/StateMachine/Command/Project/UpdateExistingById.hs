{-|
Module      : Test.Cascade.Api.StateMachine.Command.Project.UpdateExistingById
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Test.Cascade.Api.StateMachine.Command.Project.UpdateExistingById
    ( updateExistingById
    ) where

import qualified Cascade.Api.Data.Project                            as Project
import qualified Cascade.Api.Hedgehog.Gen.Api.Project                as Gen
import qualified Cascade.Api.Network.TestClient.Api.Projects         as Cascade.Api.Projects
import qualified Cascade.Api.Servant.Response                        as Response
import           Cascade.Api.Test.Prelude
import           Control.Lens                                        ( has, ix, (%~), (^.) )
import           Hedgehog
import qualified Hedgehog.Gen                                        as Gen
import           Test.Cascade.Api.StateMachine.Command.Project.Types
import           Test.Cascade.Api.StateMachine.Model                 ( Model )
import qualified Test.Cascade.Api.StateMachine.Model                 as Model

updateExistingById :: forall g m . MonadGen g => MonadIO m => MonadTest m => Command g m Model
updateExistingById = Command generator execute [Require require, Update update]

generator :: forall g . MonadGen g => Model Symbolic -> Maybe (g (UpdateById Symbolic))
generator model = case updateByIds of
  [] -> Nothing
  _  -> Just <| Gen.choice updateByIds
 where
  updateByIds :: [g (UpdateById Symbolic)]
  updateByIds = model |> Model.mapUsernameTokenProjectIdAList \(username, token, id) -> do
    updatable <- Gen.project
    pure UpdateById { .. }

require :: Model Symbolic -> UpdateById Symbolic -> Bool
require model UpdateById { username, id } = doesProjectCreated && doesUserHasAuthToken
 where
  doesProjectCreated   = model |> has (#project . #byUsername . ix username . ix id)
  doesUserHasAuthToken = model |> has (#authToken . #byUsername . ix username)

execute :: MonadIO m => MonadTest m => UpdateById Concrete -> m ()
execute input@UpdateById { token, id, updatable } = do
  label "[Project/Update Existing By ID]"
  response <- evalIO $ Cascade.Api.Projects.updateById (concrete token) (concrete id) updatable
  ensure input response

update :: Ord1 v => Model v -> UpdateById v -> Var () v -> Model v
update model UpdateById { username, id, updatable } _ =
  model |> (#project . #byUsername . ix username . ix id) %~ Cascade.Api.Projects.updateCreatable updatable

ensure :: MonadTest m => UpdateById Concrete -> Cascade.Api.Projects.UpdateByIdResponse -> m ()
ensure UpdateById { id } response = do
  footnoteShow response
  Response.Ok readable <- (response ^. #responseBody) |> evalUnion @(Response.Ok Project.Readable)
  readable ^. #id === concrete id
