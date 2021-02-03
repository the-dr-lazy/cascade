module Cascade.Api.Database.User
  ( UserTable(..)
  , Row
  ) where

import qualified Cascade.Api.Data.User         as User
import qualified Cascade.Api.Data.WrappedC     as Wrapped
import qualified Cascade.Api.Effect.Scrypt     as Scrypt
import           Chronos                        ( Time )
import           Database.Beam                  ( Beamable
                                                , C
                                                , Table(..)
                                                )

-- brittany-disable-next-binding
data UserTable (f :: Type -> Type) = Row
  { id                :: Wrapped.C f User.Id
  , username          :: Wrapped.C f User.Username
  , emailAddress      :: Wrapped.C f User.EmailAddress
  , encryptedPassword :: Wrapped.C f (Scrypt.Encrypted User.Password)
  , createdAt         :: C f Time
  , updatedAt         :: C f Time
  }
  deriving stock Generic
  deriving anyclass Beamable

instance Table UserTable where
  newtype PrimaryKey UserTable f = PrimaryKey
    { unPrimaryKey :: Wrapped.C f User.Id }
    deriving stock Generic
    deriving anyclass Beamable
  primaryKey = PrimaryKey . id

type Row = UserTable Identity

deriving stock instance Show Row
deriving stock instance Eq Row
