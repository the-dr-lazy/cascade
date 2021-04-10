{-|
Module      : Cascade.Core.Internal.Data.Contract.Database.UserTable
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Core.Internal.Data.Contract.Database.UserTable (UserTable(..), PrimaryKey(..), Row) where

import qualified Cascade.Core.Data.Model.Hashed     as Hashed
import qualified Cascade.Core.Data.Model.Phase      as Phase
import           Cascade.Core.Data.Model.User        ( User )
import qualified Cascade.Core.Data.Model.User       as Model
import qualified Cascade.Core.Data.Model.User.EmailAddress
                                                    as EmailAddress
import qualified Cascade.Core.Data.Model.User.Username
                                                    as Username
import           Cascade.Core.Internal.Orphans       ( )
import           Chronos                             ( Time )
import           Database.Beam                       ( Beamable
                                                     , C
                                                     , Table(..)
                                                     )

data UserTable f = Row
  { id             :: C f UUID
  , username       :: C f Text
  , emailAddress   :: C f Text
  , hashedPassword :: C f ByteString
  , createdAt      :: C f Time
  , updatedAt      :: C f Time
  }
  deriving stock Generic
  deriving anyclass Beamable

instance Table UserTable where
  newtype PrimaryKey UserTable f = PrimaryKey { unPrimaryKey :: C f UUID }
    deriving stock Generic
    deriving anyclass Beamable
  primaryKey = PrimaryKey . id

deriving newtype instance Show (PrimaryKey UserTable Identity)
deriving newtype instance Eq (PrimaryKey UserTable Identity)

type Row = UserTable Identity

deriving stock instance Show Row
deriving stock instance Eq Row
