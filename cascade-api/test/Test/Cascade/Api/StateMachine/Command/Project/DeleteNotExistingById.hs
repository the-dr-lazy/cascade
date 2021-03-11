{-|
Module      : Test.Cascade.Api.StateMachine.Command.Project.DeleteNotExistingById
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Test.Cascade.Api.StateMachine.Command.Project.DeleteNotExistingById
  ( deleteNotExistingById
  ) where

import qualified Cascade.Api.Network.TestClient.Api.Projects
                                               as Cascade.Api.Projects
import           Cascade.Api.Test.Prelude       ( )
import           Control.Lens                   ( (^.)
                                                , (^@..)
                                                , has
                                                , ix
                                                )
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import           Test.Cascade.Api.StateMachine.Command.Project.Types
import           Test.Cascade.Api.StateMachine.Model
                                                ( Model )
import qualified Test.Cascade.Api.StateMachine.Model.Lens
                                               as Model.Lens

deleteNotExistingById :: MonadGen g
                      => MonadIO m => MonadTest m => Command g m Model
deleteNotExistingById =
  Command generator execute [Require require, Ensure \_ _ _ -> ensure]

generator :: MonadGen g => Model Symbolic -> Maybe (g (DeleteById Symbolic))
generator model = case deleteByIds of
  [] -> Nothing
  _  -> Just <| Gen.element deleteByIds
 where
  deleteByIds = do
    (username, token) <- model ^@.. Model.Lens.indexTokenByUsername
    id                <- model ^. #project . #notExistingIds
    pure DeleteById { .. }

require :: Model Symbolic -> DeleteById Symbolic -> Bool
require model DeleteById { username } =
  model |> has (#authToken . #byUsername . ix username)

execute :: MonadIO m
        => MonadTest m
        => DeleteById Concrete
        -> m Cascade.Api.Projects.DeleteByIdResponse
execute DeleteById { token, id } = do
  label "[Project/Delete Not Existing By ID]"
  evalIO $ Cascade.Api.Projects.deleteById (concrete token) (concrete id)

ensure :: Cascade.Api.Projects.DeleteByIdResponse -> Test ()
ensure response = do
  footnoteShow response
  response ^. #responseStatusCode . #statusCode === 404
