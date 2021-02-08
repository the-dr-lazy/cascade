{-|
Module      : Cascade.Api.Effect.Database.User
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Effect.Database.User
  ( UserL
  , findByUsername
  , create
  , doesExistsByUsernameOrEmailAddress
  , run
  ) where

import           Cascade.Api.Data.ByteString.Password
                                                ( Password )
import qualified Cascade.Api.Data.User         as User
import           Cascade.Api.Data.WrappedC
import qualified Cascade.Api.Database.UserTable
                                               as UserTable
import           Cascade.Api.Database.UserTable ( UserTable )
import qualified Cascade.Api.Effect.Database   as Database
import           Cascade.Api.Effect.Database    ( DatabaseL )
import qualified Cascade.Api.Effect.Scrypt     as Scrypt
import           Cascade.Api.Effect.Scrypt      ( ScryptL )
import qualified Cascade.Api.Query             as Query
import qualified Cascade.Api.Query.User        as User
import           Control.Lens                   ( (^.)
                                                , to
                                                )
import           Database.Beam                  ( default_
                                                , insertExpressions
                                                , select
                                                , val_
                                                )
import qualified Database.Beam                 as Beam
import           Database.Beam.Backend          ( BeamSqlBackend
                                                , BeamSqlBackendCanSerialize
                                                )
import           Polysemy                       ( Members
                                                , Sem
                                                , interpret
                                                , makeSem
                                                )
import qualified Relude.Unsafe                 as Unsafe

data UserL m a where
  FindByUsername ::User.Username -> UserL m (Maybe UserTable.Row)
  DoesExistsByUsernameOrEmailAddress ::User.Username -> User.EmailAddress -> UserL m Bool
  Create ::User.ParsedCreatable -> UserL m User.Readable

makeSem ''UserL

run :: Beam.HasQBuilder backend
    => Beam.FromBackendRow backend Bool
    => Query.TableFieldsFulfillConstraints
         '[ Beam.FromBackendRow backend
          , Beam.HasSqlEqualityCheck backend
          , BeamSqlBackendCanSerialize backend
          ]
         UserTable
    => Members '[ScryptL , DatabaseL backend] r
    => Sem (UserL ': r) a
    -> Sem r a
run = interpret \case
  FindByUsername username ->
    User.queryByUsername username |> select |> Database.runSelectReturningOne
  DoesExistsByUsernameOrEmailAddress username emailAddress -> do
    User.queryExistanceByUsernameOrEmailAddress username emailAddress
      |> select
      |> Database.runSelectReturningOne
      -- Only @Just@ is acceptable.
      |> fmap Unsafe.fromJust
  Create creatable -> do
    encryptedPassword <- creatable ^. #password |> Scrypt.encryptPassword
    insertExpressions [fromParsedCreatableUser encryptedPassword creatable]
      |> Query.insert #users
      |> Database.runInsertReturningOne
      -- Only @Just@ is acceptable.
      |> fmap Unsafe.fromJust
      |> fmap toReadableUser

fromParsedCreatableUser :: BeamSqlBackend backend
                        => Query.TableFieldsFulfillConstraint
                             (BeamSqlBackendCanSerialize backend)
                             UserTable
                        => Scrypt.Encrypted Password
                        -> User.ParsedCreatable
                        -> UserTable (Beam.QExpr backend s)
fromParsedCreatableUser encryptedPassword creatable = UserTable.Row
  { id                = default_
  , username          = creatable ^. #username . to WrappedC . to val_
  , emailAddress      = creatable ^. #emailAddress . to WrappedC . to val_
  , encryptedPassword = val_ . WrappedC $ encryptedPassword
  , createdAt         = default_
  , updatedAt         = default_
  }

toReadableUser :: UserTable.Row -> User.Readable
toReadableUser UserTable.Row {..} = User.Readable
  { id           = coerce id
  , username     = coerce username
  , emailAddress = coerce emailAddress
  }
