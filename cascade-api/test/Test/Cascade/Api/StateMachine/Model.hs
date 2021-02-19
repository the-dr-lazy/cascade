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

module Test.Cascade.Api.StateMachine.Model (Model, initialModel) where

import qualified Cascade.Api.Data.Project           as Project
import qualified Cascade.Api.Data.User              as User
import qualified Cascade.Api.Data.Task              as Task
import           Data.Generics.Labels                ( )
import qualified Data.Map.Strict                    as Map
import           Hedgehog.Internal.State             ( Var )

-- brittany-disable-next-binding
data Model (v :: Type -> Type) = Model
  { project :: ProjectModel v
  , user    :: UserModel v
  , task    :: TaskModel v
  }
  deriving stock Generic

initialModel :: Model v
initialModel = Model { project = ProjectModel { creatables = Map.empty, notExistingIds = mempty }
                     , user    = UserModel { byUsername = Map.empty, byEmailAddress = Map.empty }
                     , task    = TaskModel { creatables = Map.empty, byProjectId = Map.empty, notExistingIds = mempty }
                     }

-- brittany-disable-next-binding
data ProjectModel (v :: Type -> Type) = ProjectModel
  { creatables     :: Map (Var Project.Id v) Project.Creatable
  , notExistingIds :: [Var Project.Id v]
  }
  deriving stock Generic

-- brittany-disable-next-binding
data UserModel (v :: Type -> Type) = UserModel
  { byUsername     :: Map Text User.RawCreatable
  , byEmailAddress :: Map Text User.RawCreatable
  }
  deriving stock Generic

-- brittany-disable-next-binding
data TaskModel (v :: Type -> Type) = TaskModel
  { creatables     :: Map (Var Task.Id v) Task.RawCreatable
  , byProjectId    :: Map (Var Project.Id v) [Task.RawCreatable]
  , notExistingIds :: [Var Task.Id v]
  }
  deriving stock Generic
