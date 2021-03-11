{-|
Module      : Test.Cascade.Api.StateMachine.Command.Project.Types
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Test.Cascade.Api.StateMachine.Command.Project.Types
  ( GetById(..)
  , UpdateById(..)
  , DeleteById(..)
  ) where

import qualified Cascade.Api.Data.Project      as Project
import           Cascade.Api.Network.TestClient ( AuthToken )
import           Data.Generics.Labels           ( )
import           Hedgehog
import           Test.Cascade.Api.StateMachine.Model

-- brittany-disable-next-binding
data GetById (v :: Type -> Type) = GetById
  { username :: Username
  , token    :: Var AuthToken v
  , id       :: Var Project.Id v
  }
  deriving stock Show

instance HTraversable GetById where
  htraverse f GetById {..} =
    GetById username <$> htraverse f token <*> htraverse f id

-- brittany-disable-next-binding
data UpdateById (v :: Type -> Type) = UpdateById
  { updatable :: Project.Updatable
  , username  :: Username
  , token     :: Var AuthToken v
  , id        :: Var Project.Id v
  }
  deriving stock (Generic, Show)

instance HTraversable UpdateById where
  htraverse f UpdateById {..} =
    UpdateById updatable username <$> htraverse f token <*> htraverse f id

-- brittany-disable-next-binding
data DeleteById (v :: Type -> Type) = DeleteById
  { username :: Username
  , token :: Var AuthToken v
  , id :: Var Project.Id v
  }
  deriving stock (Generic, Show)

instance HTraversable DeleteById where
  htraverse f DeleteById {..} =
    DeleteById username <$> htraverse f token <*> htraverse f id
