{-|
Module      : Cascade.Api.Database.Project
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Database.Project (ProjectTable(..), PrimaryKey(..), Row) where

import qualified Cascade.Api.Data.Project           as Project
import qualified Cascade.Api.Data.WrappedC          as Wrapped
import           Control.Lens                        ( _Wrapped'
                                                     , view
                                                     )
import           Data.Generics.Labels                ( )
import           Database.Beam                       ( Beamable
                                                     , C
                                                     , PrimaryKey
                                                     , Table(..)
                                                     )

-- brittany-disable-next-binding
data ProjectTable (f :: Type -> Type) = Row
  { id   :: Wrapped.C f Project.Id
  , name :: C f Text
  }
  deriving stock Generic
  deriving anyclass Beamable

instance Table ProjectTable where
  newtype PrimaryKey ProjectTable f = PrimaryKey
    { unPrimaryKey :: Wrapped.C f Project.Id
    }
    deriving stock (Generic)
    deriving anyclass Beamable
  primaryKey = PrimaryKey . id

type Row = ProjectTable Identity

deriving stock instance Show Row
deriving stock instance Eq Row

deriving stock instance Show (PrimaryKey ProjectTable Identity)
deriving stock instance Eq (PrimaryKey ProjectTable Identity)
