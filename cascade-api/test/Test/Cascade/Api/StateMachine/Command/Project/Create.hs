{-|
Module      : Test.Cascade.Api.StateMachine.Command.Project.Create
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Test.Cascade.Api.StateMachine.Command.Project.Create
    ( create
    ) where

import qualified Cascade.Api.Data.Project                         as Project
import qualified Cascade.Api.Hedgehog.Gen.Api.Project             as Gen
import           Cascade.Api.Network.TestClient                   ( AuthToken )
import qualified Cascade.Api.Network.TestClient.Api.User.Projects as Cascade.Api.User.Projects
import qualified Cascade.Api.Servant.Response                     as Response
import           Cascade.Api.Test.Prelude                         ( evalUnion )
import           Control.Lens
    ( at, has, ix, non, (?~), (^.), (^@..) )
import           Data.Generics.Labels                             ()
import qualified Data.Map.Strict                                  as Map
import           Hedgehog
import qualified Hedgehog.Gen                                     as Gen
import           Test.Cascade.Api.StateMachine.Model              ( Model )
import qualified Test.Cascade.Api.StateMachine.Model.Lens         as Model.Lens

create :: MonadGen g => MonadIO m => MonadTest m => Command g m Model
create = Command generator execute [Require require, Update update]


data Create (v :: Type -> Type) = Create { creatable :: Project.Creatable
                                         , username  :: Text
                                         , token     :: Var AuthToken v
                                         }
  deriving stock (Generic, Show)

instance HTraversable Create where
  htraverse f Create {..} = Create creatable username <$> htraverse f token

generator :: MonadGen g => Model Symbolic -> Maybe (g (Create Symbolic))
generator model = case model ^@.. Model.Lens.indexTokenByUsername of
  []                 -> Nothing
  usernameTokenAList -> Just do
    creatable         <- Gen.project
    (username, token) <- Gen.element usernameTokenAList
    pure $ Create { .. }

require :: Model Symbolic -> Create Symbolic -> Bool
require model Create { username } = model |> has (#authToken . #byUsername . ix username)

execute :: MonadIO m => MonadTest m => Create Concrete -> m Project.Id
execute Create { creatable, token } = do
  label "[Project/Create]"
  response <- evalIO $ Cascade.Api.User.Projects.create (concrete token) creatable
  readable <- ensure response
  pure $ readable ^. #id

update :: Ord1 v => Model v -> Create v -> Var Project.Id v -> Model v
update model Create { username, creatable } projectId =
  model |> (#project . #byUsername . at username . non Map.empty . at projectId) ?~ creatable

ensure :: MonadTest m => Cascade.Api.User.Projects.CreateResponse -> m Project.Readable
ensure response = do
  Response.Created project <- (response ^. #responseBody) |> evalUnion @(Response.Created Project.Readable)
  pure project
