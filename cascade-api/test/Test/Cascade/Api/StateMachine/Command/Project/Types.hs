{-|
Module      : Test.Cascade.Api.StateMachine.Command.Project.Types
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2022 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Test.Cascade.Api.StateMachine.Command.Project.Types
    ( DeleteById (..)
    , GetById (..)
    , UpdateById (..)
    ) where

import qualified Cascade.Api.Data.Project            as Project
import           Cascade.Api.Network.TestClient      ( AuthToken )
import           Data.Generics.Labels                ()
import           Hedgehog
import           Test.Cascade.Api.StateMachine.Model


data GetById (v :: Type -> Type) = GetById { username :: Username
                                           , token    :: Var AuthToken v
                                           , id       :: Var Project.Id v
                                           }
  deriving stock (Generic, Show)

instance FunctorB GetById
instance TraversableB GetById

data UpdateById (v :: Type -> Type) = UpdateById { updatable :: Project.Updatable
                                                 , username  :: Username
                                                 , token     :: Var AuthToken v
                                                 , id        :: Var Project.Id v
                                                 }
  deriving stock (Generic, Show)

instance FunctorB UpdateById
instance TraversableB UpdateById

data DeleteById (v :: Type -> Type) = DeleteById { username :: Username
                                                 , token    :: Var AuthToken v
                                                 , id       :: Var Project.Id v
                                                 }
  deriving stock (Generic, Show)

instance FunctorB DeleteById
instance TraversableB DeleteById
