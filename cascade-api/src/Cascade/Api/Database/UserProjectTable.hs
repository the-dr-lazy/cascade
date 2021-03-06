{-|
Module      : Cascade.Api.Database.UserProjectTable
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Database.UserProjectTable
    ( PrimaryKey (..)
    , Row
    , UserProjectTable (..)
    ) where

import           Cascade.Api.Database.ProjectTable ( ProjectTable )
import           Cascade.Api.Database.UserTable    ( UserTable )
import           Chronos                           ( OffsetDatetime )
import           Database.Beam                     ( Beamable, C, Table (..) )


data UserProjectTable (f :: Type -> Type) = Row { userId    :: PrimaryKey UserTable f
                                                , projectId :: PrimaryKey ProjectTable f
                                                , createdAt :: C f OffsetDatetime
                                                , updatedAt :: C f OffsetDatetime
                                                }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table UserProjectTable where
  data PrimaryKey UserProjectTable f = PrimaryKey (PrimaryKey UserTable f) (PrimaryKey ProjectTable f)
    deriving stock Generic
    deriving anyclass Beamable
  primaryKey = PrimaryKey <$> userId <*> projectId

type Row = UserProjectTable Identity

deriving stock instance Show Row
deriving stock instance Eq Row
