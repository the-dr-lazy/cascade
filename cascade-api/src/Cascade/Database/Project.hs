module Cascade.Database.Project
  ( ProjectTable(..)
  , PrimaryKey(..)
  , Row
  ) where

import qualified Cascade.Data.Api.Project      as Project
import           Data.Generics.Labels           ( )
import           Database.Beam                  ( Beamable
                                                , C
                                                , PrimaryKey
                                                , Table(..)
                                                )

-- brittany-disable-next-binding
data ProjectTable (f :: Type -> Type) = Row
  { id   :: C f Project.Id
  , name :: C f Text
  }
  deriving stock Generic
  deriving anyclass Beamable

instance Table ProjectTable where
  newtype PrimaryKey ProjectTable f = PrimaryKey
    { unPrimaryKey :: C f Project.Id
    }
    deriving stock Generic
    deriving anyclass Beamable
  primaryKey = PrimaryKey . id

type Row = ProjectTable Identity

deriving stock instance Show Row
deriving stock instance Eq Row
