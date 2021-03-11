{-|
Module      : Test.Cascade.Api.StateMachine.Command.Project.GetNotExistingById
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Test.Cascade.Api.StateMachine.Command.Project.GetNotExistingById (getNotExistingById) where

import qualified Cascade.Api.Network.TestClient.Api.Projects
                                                    as Cascade.Api.Projects
import           Cascade.Api.Test.Prelude            ( )
import           Control.Lens                        ( (^.)
                                                     , (^@..)
                                                     , has
                                                     , ix
                                                     )
import           Hedgehog
import qualified Hedgehog.Gen                       as Gen
import           Test.Cascade.Api.StateMachine.Command.Project.Types
import           Test.Cascade.Api.StateMachine.Model ( Model )
import qualified Test.Cascade.Api.StateMachine.Model.Lens
                                                    as Model.Lens

getNotExistingById :: MonadGen g => MonadIO m => MonadTest m => Command g m Model
getNotExistingById = Command generator execute [Require require, Ensure \_ _ _ -> ensure]

generator :: MonadGen g => Model Symbolic -> Maybe (g (GetById Symbolic))
generator model = case getByIds of
  [] -> Nothing
  _  -> Just <| Gen.element getByIds
 where
  getByIds = do
    (username, token) <- model ^@.. Model.Lens.indexTokenByUsername
    id                <- model ^. #project . #notExistingIds
    pure GetById { .. }

require :: Model Symbolic -> GetById Symbolic -> Bool
require model GetById { username } = model |> has (#authToken . #byUsername . ix username)

execute :: MonadIO m => MonadTest m => GetById Concrete -> m Cascade.Api.Projects.GetByIdResponse
execute GetById { token, id } = do
  label "[Project/Get Not Existing By ID]"
  evalIO $ Cascade.Api.Projects.getById (concrete token) (concrete id)

ensure :: Cascade.Api.Projects.GetByIdResponse -> Test ()
ensure response = do
  footnoteShow response
  response ^. #responseStatusCode . #statusCode === 404
