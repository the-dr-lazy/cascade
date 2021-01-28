module Cascade.Api.Database.Task
  ( ProjectTable(..)
  , PrimaryKey(..)
  , Row
  ) where

import qualified Cascade.Api.Data.Task as Task
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

import           Cascade.Api.Database.Project   ( ProjectTable )
import           Chronos                        ( Time )

-- brittany-disable-next-binding
data TaskTable (f :: Type -> Type) = Row
  { id         :: C f (WrappedC Task.TaskId)
  , title      :: C f Text
  , deadlineAt :: C f Time
  , projectId  :: PrimaryKey ProjectTable f
  }
  deriving stock Generic
  deriving anyclass Beamable

instance Table TaskTable where
  newtype PrimaryKey TaskTable f = PrimaryKey
    { unPrimaryKey :: C f (WrappedC Task.TaskId)
    }
    deriving stock Generic
    deriving anyclass Beamable
  primaryKey = PrimaryKey . id


type Row = TaskTable Identity

-- deriving stock instance Show Row
-- deriving stock instance Eq Row

