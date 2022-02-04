{-|
Module      : Test.Cascade.Api.StateMachine.Command.Project.GetAll
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2022 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Test.Cascade.Api.StateMachine.Command.Project.GetAll
    ( getAll
    ) where

import qualified Cascade.Api.Data.Project                         as Project
import           Cascade.Api.Network.TestClient                   ( AuthToken )
import qualified Cascade.Api.Network.TestClient.Api.Projects      as Cascade.Api.Projects
import qualified Cascade.Api.Network.TestClient.Api.User.Projects as Cascade.Api.User.Projects
import qualified Cascade.Api.Servant.Response                     as Response
import           Cascade.Api.Test.Prelude
import           Control.Lens                                     ( at, has, ix, non, (^.), (^@..) )
import           Data.Generics.Labels                             ()
import qualified Data.Map.Strict                                  as Map
import           Hedgehog
import qualified Hedgehog.Gen                                     as Gen
import           Prelude                                          hiding ( getAll )
import           Test.Cascade.Api.StateMachine.Model              ( Model )
import qualified Test.Cascade.Api.StateMachine.Model.Lens         as Model.Lens
getAll :: forall g m . MonadGen g => MonadIO m => MonadTest m => Command g m Model
getAll = Command generator execute [Require require, Ensure ensure]


data GetAll (v :: Type -> Type) = GetAll { username :: Text
                                         , token    :: Var AuthToken v
                                         }
  deriving stock (Generic, Show)

instance HTraversable GetAll where
  htraverse f GetAll {..} = GetAll username <$> htraverse f token

generator :: MonadGen g => Model Symbolic -> Maybe (g (GetAll Symbolic))
generator model = case model ^@.. Model.Lens.indexTokenByUsername of
  []                 -> Nothing
  usernameTokenAList -> Just $ uncurry GetAll <$> Gen.element usernameTokenAList

require :: Model Symbolic -> GetAll Symbolic -> Bool
require model GetAll { username } = model |> has (#authToken . #byUsername . ix username)

execute :: MonadIO m => MonadTest m => GetAll Concrete -> m Cascade.Api.User.Projects.GetAllResponse
execute GetAll { token } = do
  label "[Project/Get All]"
  evalIO $ Cascade.Api.User.Projects.getAll (concrete token)

ensure :: Model Concrete -> Model Concrete -> GetAll Concrete -> Cascade.Api.User.Projects.GetAllResponse -> Test ()
ensure before _ GetAll { username } response = do
  footnoteShow response
  Response.Ok readables <- (response ^. #responseBody) |> evalUnion @(Response.Ok [Project.Readable])

  let projects = before ^. #project . #byUsername . at username . non Map.empty
  length readables === Map.size projects

  for_ readables $ \readable -> do
    let id = Var . Concrete $ readable ^. #id
    (Map.lookup id projects |> evalMaybe) >>= Cascade.Api.Projects.testReadableVsCreatable readable
