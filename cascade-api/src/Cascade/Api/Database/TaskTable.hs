{-|
Module      : Cascade.Api.Database.TaskTable
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Database.TaskTable (TaskTable(..), PrimaryKey(..), Row) where

import qualified Cascade.Api.Data.Task              as Task
import qualified Cascade.Api.Data.WrappedC          as Wrapped
import           Cascade.Api.Database.ProjectTable   ( ProjectTable )
import qualified Cascade.Data.Text                  as Text
import           Chronos                             ( OffsetDatetime )
import           Data.Generics.Labels                ( )
import           Database.Beam                       ( Beamable
                                                     , C
                                                     , PrimaryKey
                                                     , Table(..)
                                                     )

-- brittany-disable-next-binding
data TaskTable (f :: Type -> Type) = Row
  { id         :: Wrapped.C f Task.Id
  , title      :: Wrapped.C f Text.NonEmpty
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
