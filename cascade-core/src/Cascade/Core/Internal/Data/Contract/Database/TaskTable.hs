{-|
Module      : Cascade.Core.Internal.Data.Contract.Database.TaskTable
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Core.Internal.Data.Contract.Database.TaskTable (TaskTable(..), PrimaryKey(..), Row) where

import           Cascade.Core.Internal.Data.Contract.Database.ProjectTable
                                                     ( ProjectTable )
import           Chronos                             ( Time )
import           Data.Generics.Labels                ( )
import           Database.Beam                       ( Beamable
                                                     , C
                                                     , PrimaryKey
                                                     , Table(..)
                                                     )

-- brittany-disable-next-binding
data TaskTable (f :: Type -> Type) = Row
  { id         :: C f UUID
  , title      :: C f Text
  , deadlineAt :: C f Time
  , projectId  :: PrimaryKey ProjectTable f
  }
  deriving stock Generic
  deriving anyclass Beamable

instance Table TaskTable where
  newtype PrimaryKey TaskTable f = PrimaryKey
    { unPrimaryKey :: C f UUID
    }
    deriving stock Generic
    deriving anyclass Beamable
  primaryKey = PrimaryKey . id

type Row = TaskTable Identity

deriving stock instance Show Row
deriving stock instance Eq Row
