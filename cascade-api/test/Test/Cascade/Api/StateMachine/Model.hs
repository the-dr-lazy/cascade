{-|
Module      : Test.Cascade.Api.StateMachine.Model
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Test.Cascade.Api.StateMachine.Model
    ( EmailAddress
    , Model
    , Password
    , Username
    , getUsernameTokenProjectIdAList
    , initialModel
    , mapUsernameTokenProjectIdAList
    ) where

import qualified Cascade.Api.Data.Project       as Project
import qualified Cascade.Api.Data.Task          as Task
import qualified Cascade.Api.Data.User          as User
import           Cascade.Api.Network.TestClient ( AuthToken )
import qualified Cascade.Data.Validation        as Validation
import           Control.Lens                   ( at, foldMapOf, folded, to, (^..) )
import           Data.Generics.Labels           ()
import qualified Data.Map.Strict                as Map
import           Hedgehog.Internal.State        ( Var )


data Model (v :: Type -> Type) = Model { project   :: ProjectModel v
                                       , user      :: UserModel
                                       , task      :: TaskModel v
                                       , authToken :: AuthTokenModel v
                                       }
  deriving stock (Generic)

initialModel :: Model v
initialModel = Model { authToken = AuthTokenModel { byUsername = Map.empty }
                     , project   = ProjectModel { byUsername = Map.empty, notExistingIds = mempty }
                     , task      = TaskModel { byProjectId = Map.empty, notExistingIds = mempty }
                     , user      = UserModel { byUsername = Map.empty, byEmailAddress = Map.empty }
                     }

type Username = Text
type EmailAddress = Text
type Password = Text


data AuthTokenModel (v :: Type -> Type) = AuthTokenModel { byUsername :: Map Text (Var AuthToken v)
                                                         }
  deriving stock (Generic)


data ProjectModel (v :: Type -> Type) = ProjectModel { notExistingIds :: [Var Project.Id v]
                                                     , byUsername :: Map Username (Map (Var Project.Id v) Project.Creatable)
                                                     }
  deriving stock (Generic)


data TaskModel (v :: Type -> Type) = TaskModel { byProjectId :: Map (Var Project.Id v) (Map (Var Task.Id v) (Task.Creatable Validation.Raw))
                                               , notExistingIds :: [Var Task.Id v]
                                               }
  deriving stock (Generic)

data UserModel = UserModel { byUsername     :: Map Username (User.Creatable Validation.Raw)
                           , byEmailAddress :: Map EmailAddress (User.Creatable Validation.Raw)
                           }
  deriving stock (Generic)

mapUsernameTokenProjectIdAList :: ((Username, Var AuthToken v, Var Project.Id v) -> a) -> Model v -> [a]
mapUsernameTokenProjectIdAList f model = model |> foldMapOf
  (#project . #byUsername . to Map.toList . folded)
  \(username, byProjectId) -> do
    token     <- model ^.. #authToken . #byUsername . at username . folded
    projectId <- Map.keys byProjectId
    pure $ f (username, token, projectId)

getUsernameTokenProjectIdAList :: Model v -> [(Username, Var AuthToken v, Var Project.Id v)]
getUsernameTokenProjectIdAList = mapUsernameTokenProjectIdAList identity
