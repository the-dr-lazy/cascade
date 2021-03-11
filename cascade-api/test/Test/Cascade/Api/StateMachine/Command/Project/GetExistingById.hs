{-|
Module      : Test.Cascade.Api.StateMachine.Command.Project.GetExistingById
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Test.Cascade.Api.StateMachine.Command.Project.GetExistingById
  ( getExistingById
  ) where

import qualified Cascade.Api.Data.Project      as Project
import qualified Cascade.Api.Network.TestClient.Api.Projects
                                               as Cascade.Api.Projects
import qualified Cascade.Api.Servant.Response  as Response
import           Cascade.Api.Test.Prelude
import           Control.Lens                   ( (^.)
                                                , (^?)
                                                , has
                                                , ix
                                                )
import           Data.Generics.Labels           ( )
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import           Test.Cascade.Api.StateMachine.Command.Project.Types
import           Test.Cascade.Api.StateMachine.Model
                                                ( Model )
import qualified Test.Cascade.Api.StateMachine.Model
                                               as Model

getExistingById :: MonadGen g => MonadIO m => MonadTest m => Command g m Model
getExistingById = Command generator execute [Require require, Ensure ensure]

generator :: MonadGen g => Model Symbolic -> Maybe (g (GetById Symbolic))
generator model = case getByIds of
  [] -> Nothing
  _  -> Just <| Gen.element getByIds
 where
  getByIds =
    model |> Model.mapUsernameTokenProjectIdAList \(username, token, id) ->
      GetById { .. }


require :: Model Symbolic -> GetById Symbolic -> Bool
require model GetById { id, username } = isProjectCreated && hasUserAuthToken
 where
  isProjectCreated =
    model |> has (#project . #byUsername . ix username . ix id)
  hasUserAuthToken = model |> has (#authToken . #byUsername . ix username)

execute :: MonadIO m
        => MonadTest m
        => GetById Concrete
        -> m Cascade.Api.Projects.GetByIdResponse
execute GetById { token, id } = do
  label "[Project/Get Existing By ID]"
  evalIO $ Cascade.Api.Projects.getById (concrete token) (concrete id)

ensure :: Model Concrete
       -> Model Concrete
       -> GetById Concrete
       -> Cascade.Api.Projects.GetByIdResponse
       -> Test ()
ensure before _ GetById { username, id = projectId } response = do
  footnoteShow response
  Response.Ok readable <-
    (response ^. #responseBody) |> evalUnion @(Response.Ok Project.Readable)
  projectId === (Var . Concrete $ readable ^. #id)
  evalMaybe mCreatable >>= Cascade.Api.Projects.testReadableVsCreatable readable
 where
  mCreatable = before ^? #project . #byUsername . ix username . ix projectId
