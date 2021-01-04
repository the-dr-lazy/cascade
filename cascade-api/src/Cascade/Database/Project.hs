module Cascade.Database.Project
  ( ProjectTable
  , PrimaryKey(..)
  , Id
  , Project
  ) where

import           Data.Generics.Labels           ( )
import           Data.UUID                      ( UUID )
import           Database.Beam                  ( Beamable
                                                , C
                                                , PrimaryKey
                                                , Table
                                                )
import qualified Database.Beam                 as Beam

-- brittany-disable-next-binding
data ProjectTable (f :: Type -> Type) = Project
  { id   :: C f UUID
  , name :: C f Text
  }
  deriving stock Generic
  deriving anyclass Beamable

instance Table ProjectTable where
  data PrimaryKey ProjectTable f = Id
    { unId :: C f UUID
    }
    deriving stock Generic
    deriving anyclass Beamable
  primaryKey = Id . id


type Id = PrimaryKey ProjectTable Identity


type Project = ProjectTable Identity

deriving stock instance Show Project
deriving stock instance Eq Project
