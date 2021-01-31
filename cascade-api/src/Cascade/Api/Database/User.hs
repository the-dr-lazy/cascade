module Cascade.Api.Database.User where

import qualified Cascade.Api.Data.User         as User
import qualified Cascade.Api.Data.WrappedC     as Wrapped
import           Chronos                        ( Time )
import           Crypto.Scrypt                  ( PassHash )
import           Database.Beam                  ( Beamable
                                                , C
                                                , Table(..)
                                                )

-- brittany-disable-next-binding
data UserTable (f :: Type -> Type) = Row
  { id           :: Wrapped.C f User.Id
  , username     :: Wrapped.C f User.Username
  , emailAddress :: Wrapped.C f User.EmailAddress
  , passHash     :: Wrapped.C f PassHash
  , createdAt    :: C f Time
  , updatedAt    :: C f Time
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
