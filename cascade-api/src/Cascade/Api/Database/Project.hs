module Cascade.Api.Database.Project
  ( ProjectTable(..)
  , PrimaryKey(..)
  , Row
  ) where

import qualified Cascade.Api.Data.Project      as Project
import           Cascade.Api.Data.WrappedC
import           Control.Lens                   ( _Wrapped'
                                                , view
                                                )
import           Data.Generics.Labels           ( )
import           Database.Beam                  ( Beamable
                                                , C
                                                , PrimaryKey
                                                , Table(..)
                                                )

-- brittany-disable-next-binding
data ProjectTable (f :: Type -> Type) = Row
  { id   :: C f (WrappedC Project.Id)
  , name :: C f Text
  }
  deriving stock Generic
  deriving anyclass Beamable

instance Table ProjectTable where
  newtype PrimaryKey ProjectTable f = PrimaryKey
    { unPrimaryKey :: C f (WrappedC Project.Id)
    }
    deriving stock Generic
    deriving anyclass Beamable
  primaryKey = PrimaryKey . id

type Row = ProjectTable Identity

deriving stock instance Show Row
deriving stock instance Eq Row
