{-|
Module      : Test.Cascade.Api.StateMachine.Command.Authentication.Types
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2022 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Test.Cascade.Api.StateMachine.Command.Authentication.Types where

import qualified Cascade.Api.Data.Authentication as Authentication
import qualified Cascade.Data.Validation         as Validation
import           Hedgehog

newtype Login (v :: Type -> Type)
  = Login (Authentication.Credential 'Validation.Raw)
  deriving stock (Generic, Show)

instance FunctorB Login
instance TraversableB Login
