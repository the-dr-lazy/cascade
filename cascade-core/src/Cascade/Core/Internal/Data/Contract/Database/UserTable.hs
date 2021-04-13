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

module Cascade.Core.Internal.Data.Contract.Database.UserTable
  ( UserTable(..)
  , PrimaryKey(PrimaryKey)
  , Row
  , MalformationError
  , unPrimaryKey
  , unsafePersistedUserFromRow
  , newUserToRow
  ) where

import qualified Cascade.Core.Data.Model.Hashed     as Hashed
import qualified Cascade.Core.Data.Model.Phase      as Phase
import           Cascade.Core.Data.Model.User        ( User(..) )
import           Cascade.Core.Effect                 ( TimeL )
import qualified Cascade.Core.Effect.Time           as Time
import           Cascade.Core.Internal.Data.Contract.Database.TaskTable
                                                     ( TaskTable )
import qualified Cascade.Core.Internal.Data.Contract.Database.TaskTable
                                                    as TaskTable
import qualified Cascade.Core.Internal.Data.Model.Task.Id
                                                    as Task.Id
import qualified Cascade.Core.Internal.Data.Model.User.EmailAddress
                                                    as EmailAddress
import qualified Cascade.Core.Internal.Data.Model.User.Id
                                                    as User.Id
import qualified Cascade.Core.Internal.Data.Model.User.Username
                                                    as Username
import qualified Cascade.Data.Either                as Either
import qualified Cascade.Data.Validation            as Validation
import           Chronos                             ( Time )
import           Control.Lens                        ( (^.) )
import           Database.Beam                       ( Beamable
                                                     , C
                                                     , Nullable
                                                     , Table(..)
                                                     )
import           Polysemy                            ( Member
                                                     , Sem
                                                     )

data UserTable f = Row
  { id                 :: C f UUID
  , username           :: C f Text
  , emailAddress       :: C f Text
  , password           :: C f ByteString
  , currentWorkingTask :: PrimaryKey TaskTable (Nullable f)
  , createdAt          :: C f Time
  , updatedAt          :: C f Time
  }
  deriving stock Generic
  deriving anyclass Beamable

instance Table UserTable where
  newtype PrimaryKey UserTable f = PrimaryKey { unPrimaryKey :: C f UUID }
    deriving stock Generic
    deriving anyclass Beamable
  primaryKey Row { id } = PrimaryKey id

deriving newtype instance Show (PrimaryKey UserTable Identity)
deriving newtype instance Eq (PrimaryKey UserTable Identity)

type Row = UserTable Identity

deriving stock instance Show Row
deriving stock instance Eq Row

data MalformationError
  = MalformedUsernameError Username.Errors
  | MalformedEmailAddressError
  | MalformedPasswordHashError
  | MalformedUpdateBeforeCreation

unsafePersistedUserFromRow :: Row -> Either MalformationError (User 'Phase.Persisted)
unsafePersistedUserFromRow row = do
  username       <- Validation.toEither . fmap Username.unsafePhaseCoerce . first MalformedUsernameError . Username.mk <| row ^. #username
  emailAddress   <- maybeToRight MalformedEmailAddressError . fmap EmailAddress.unsafePhaseCoerce . EmailAddress.mk <| row ^. #emailAddress
  hashedPassword <- maybeToRight MalformedPasswordHashError . Hashed.coMk <| row ^. #password
  Either.leftUnless (row ^. #updatedAt >= row ^. #createdAt) MalformedUpdateBeforeCreation
  pure User { id                 = User.Id.unsafeMk (row ^. #id)
            , currentWorkingTask = fmap Task.Id.unsafeMk . TaskTable.unPrimaryKey <| row ^. #currentWorkingTask
            , ..
            }

newUserToRow :: Member TimeL r => User 'Phase.New -> Sem r Row
newUserToRow User {..} = do
  createdAt <- Time.now
  pure Row { id                 = User.Id.un id
           , username           = Username.un username
           , emailAddress       = EmailAddress.un emailAddress
           , password           = Hashed.un hashedPassword
           , currentWorkingTask = TaskTable.PrimaryKey <| Task.Id.un <$> currentWorkingTask
           , updatedAt          = createdAt
           , ..
           }
