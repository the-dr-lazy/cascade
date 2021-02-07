module Cascade.Api.Database.Task
  ( TaskTable(..)
  , PrimaryKey(..)
  , Row
  ) where

import qualified Cascade.Api.Data.Task         as Task
import qualified Cascade.Api.Data.WrappedC     as Wrapped
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
import           Chronos                        ( OffsetDatetime )

-- brittany-disable-next-binding
data TaskTable (f :: Type -> Type) = Row
  { id         :: Wrapped.C f Task.Id
  , title      :: C f Text
  , deadlineAt :: C f OffsetDatetime
  , projectId  :: PrimaryKey ProjectTable f
  }
  deriving stock Generic
  deriving anyclass Beamable

instance Table TaskTable where
  newtype PrimaryKey TaskTable f = PrimaryKey
    { unPrimaryKey :: Wrapped.C f Task.Id
    }
    deriving stock Generic
    deriving anyclass Beamable
  primaryKey = PrimaryKey . id


type Row = TaskTable Identity

deriving stock instance Show Row
deriving stock instance Eq Row
