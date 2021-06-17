{-|
Module      : Cascade.Api.Database.UserTable
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Database.UserTable
    ( PrimaryKey (..)
    , Row
    , UserTable (..)
    ) where

import qualified Cascade.Api.Data.User     as User
import qualified Cascade.Api.Data.WrappedC as Wrapped
import qualified Cascade.Api.Effect.Scrypt as Scrypt
import           Chronos                   (OffsetDatetime)
import           Database.Beam             (Beamable, C, Table (..))

-- brittany-disable-next-binding
data UserTable (f :: Type -> Type) = Row { id :: Wrapped.C f User.Id
                                         , username :: Wrapped.C f User.Username
                                         , emailAddress :: Wrapped.C f User.EmailAddress
                                         , encryptedPassword :: Wrapped.C f (Scrypt.Encrypted User.Password)
                                         , createdAt :: C f OffsetDatetime
                                         , updatedAt :: C f OffsetDatetime
                                         }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table UserTable where
  newtype PrimaryKey UserTable f = PrimaryKey
    { unPrimaryKey :: Wrapped.C f User.Id }
    deriving stock Generic
    deriving anyclass Beamable
  primaryKey = PrimaryKey . id

deriving newtype instance Show (PrimaryKey UserTable Identity)
deriving newtype instance Eq (PrimaryKey UserTable Identity)

type Row = UserTable Identity

deriving stock instance Show Row
deriving stock instance Eq Row
