module Test.Cascade.Api.StateMachine.Model
  ( Model
  , initialModel
  ) where

import qualified Cascade.Api.Data.Project      as Project
import           Data.Generics.Labels           ( )
import qualified Data.Map.Strict               as Map
import           Hedgehog.Internal.State        ( Var )

-- brittany-disable-next-binding
data Model (v :: Type -> Type) = Model
  { project :: ProjectModel v
  }
  deriving stock Generic

initialModel :: Model v
initialModel = Model
  { project = ProjectModel { creatables = Map.empty, notExistingIds = mempty }
  }

-- brittany-disable-next-binding
data ProjectModel (v :: Type -> Type) = ProjectModel
  { creatables     :: Map (Var Project.Id v) Project.Creatable
  , notExistingIds :: [Var Project.Id v]
  }
  deriving stock Generic
