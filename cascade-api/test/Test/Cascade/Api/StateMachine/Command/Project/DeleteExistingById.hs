{-|
Module      : Test.Cascade.Api.StateMachine.Command.Project.DeleteExistingById
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Test.Cascade.Api.StateMachine.Command.Project.DeleteExistingById
    ( deleteExistingById
    ) where

import qualified Cascade.Api.Data.Project                            as Project
import qualified Cascade.Api.Network.TestClient.Api.Projects         as Cascade.Api.Projects
import qualified Cascade.Api.Servant.Response                        as Response
import           Cascade.Api.Test.Prelude
import           Control.Lens                                        ( has, ix, sans, (%~), (^.) )
import           Hedgehog
import qualified Hedgehog.Gen                                        as Gen
import           Test.Cascade.Api.StateMachine.Command.Project.Types
import           Test.Cascade.Api.StateMachine.Model                 ( Model )
import qualified Test.Cascade.Api.StateMachine.Model                 as Model

deleteExistingById :: MonadGen g => MonadIO m => MonadTest m => Command g m Model
deleteExistingById = Command generator execute [Require require, Update update, Ensure ensure]

generator :: forall g . MonadGen g => Model Symbolic -> Maybe (g (DeleteById Symbolic))
generator model = case deleteByIds of
  [] -> Nothing
  _  -> Just <| Gen.element deleteByIds
  where deleteByIds = model |> Model.mapUsernameTokenProjectIdAList \(username, token, id) -> DeleteById { .. }

require :: Model Symbolic -> DeleteById Symbolic -> Bool
require model DeleteById { username, id } = doesProjectCreated && doesUserHasAuthToken
 where
  doesProjectCreated   = model |> has (#project . #byUsername . ix username . ix id)
  doesUserHasAuthToken = model |> has (#authToken . #byUsername . ix username)

execute :: MonadTest m => MonadIO m => DeleteById Concrete -> m Cascade.Api.Projects.DeleteByIdResponse
execute DeleteById { token, id } = do
  label "[Project/Delete Existing By ID]"
  evalIO $ Cascade.Api.Projects.deleteById (concrete token) (concrete id)

update :: Ord1 v => Model v -> DeleteById v -> Var Cascade.Api.Projects.DeleteByIdResponse v -> Model v
update model DeleteById { username, id } _ = model |> #project . #byUsername . ix username %~ sans id |> #task . #byProjectId %~ sans id

ensure :: Model Concrete -> Model Concrete -> DeleteById Concrete -> Cascade.Api.Projects.DeleteByIdResponse -> Test ()
ensure _ _ DeleteById { id } response = do
  footnoteShow response
  Response.Ok project <- (response ^. #responseBody) |> evalUnion @(Response.Ok Project.Readable)
  project ^. #id === concrete id
