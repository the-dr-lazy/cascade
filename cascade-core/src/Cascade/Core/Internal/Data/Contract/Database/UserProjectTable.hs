{-|
Module      : Cascade.Core.Internal.Data.Contract.Database.UserProjectTable
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Core.Internal.Data.Contract.Database.UserProjectTable (UserProjectTable(..), PrimaryKey(..), Row) where

import           Cascade.Core.Internal.Data.Contract.Database.ProjectTable
                                                     ( ProjectTable )
import           Cascade.Core.Internal.Data.Contract.Database.UserTable
                                                     ( UserTable )
import           Chronos                             ( OffsetDatetime )
import           Chronos                             ( Time )
import           Database.Beam                       ( Beamable
                                                     , C
                                                     , Table(..)
                                                     )

-- brittany-disable-next-binding
data UserProjectTable (f :: Type -> Type) = Row
  { userId    :: PrimaryKey UserTable f
  , projectId :: PrimaryKey ProjectTable f
  , createdAt :: C f Time
  , updatedAt :: C f Time
  }
  deriving stock Generic
  deriving anyclass Beamable

instance Table UserProjectTable where
  data PrimaryKey UserProjectTable f = PrimaryKey (PrimaryKey UserTable f) (PrimaryKey ProjectTable f)
    deriving stock Generic
    deriving anyclass Beamable
  primaryKey = PrimaryKey <$> userId <*> projectId

type Row = UserProjectTable Identity

deriving stock instance Show Row
deriving stock instance Eq Row
