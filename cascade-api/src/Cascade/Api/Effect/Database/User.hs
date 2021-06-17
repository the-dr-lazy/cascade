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
    , create
    , doesExistsByUsernameOrEmailAddress
    , findByUsername
    , run
    ) where

import           Cascade.Api.Data.ByteString.Password (Password)
import qualified Cascade.Api.Data.User                as User
import           Cascade.Api.Data.WrappedC            (WrappedC (..))
import qualified Cascade.Api.Database.Sql             as SQL
import qualified Cascade.Api.Database.Sql.Query       as SQL.Query
import qualified Cascade.Api.Database.Sql.Query.User  as SQL.Query.User
import           Cascade.Api.Database.UserTable       (UserTable)
import qualified Cascade.Api.Database.UserTable       as UserTable
import           Cascade.Api.Effect.Database          (DatabaseL)
import qualified Cascade.Api.Effect.Database          as Database
import           Cascade.Api.Effect.Scrypt            (ScryptL)
import qualified Cascade.Api.Effect.Scrypt            as Scrypt
import qualified Cascade.Data.Validation              as Validation
import           Control.Lens                         ((^.))
import           Database.Beam                        (insertExpressions)
import qualified Database.Beam                        as Beam
import           Database.Beam.Backend                (BeamSqlBackend, BeamSqlBackendCanSerialize)
import           Polysemy                             (Members, Sem, interpret, makeSem)
import qualified Relude.Unsafe                        as Unsafe

data UserL m a where
  FindByUsername :: User.Username -> UserL m (Maybe UserTable.Row)
  DoesExistsByUsernameOrEmailAddress :: User.Username -> User.EmailAddress -> UserL m Bool
  Create :: User.Creatable 'Validation.Parsed -> UserL m User.Readable

makeSem ''UserL

run :: Beam.HasQBuilder backend
    => Beam.FromBackendRow backend Bool
    => SQL.TableFieldsFulfillConstraints
         '[Beam.FromBackendRow backend , Beam.HasSqlEqualityCheck backend , BeamSqlBackendCanSerialize backend]
         UserTable
    => Members '[ScryptL , DatabaseL backend] r => Sem (UserL ': r) a -> Sem r a
run = interpret \case
  FindByUsername username                                  -> SQL.Query.User.byUsername username |> SQL.select |> Database.runSelectReturningOne
  DoesExistsByUsernameOrEmailAddress username emailAddress -> do
    let sieve = SQL.filter <| (#username `SQL.eq` SQL.literal username) `SQL.or` (#emailAddress `SQL.eq` SQL.literal emailAddress)

    SQL.Query.existance #users sieve
      |> SQL.select
      |> Database.runSelectReturningOne
      -- Only @Just@ is acceptable.
      |> fmap Unsafe.fromJust
  Create creatable -> do
    encryptedPassword <- creatable ^. #password |> Scrypt.encryptPassword
    insertExpressions [fromParsedCreatableUser encryptedPassword creatable]
      |> SQL.insert #users
      |> Database.runInsertReturningOne
      -- Only @Just@ is acceptable.
      |> fmap Unsafe.fromJust
      |> fmap toReadableUser

fromParsedCreatableUser :: BeamSqlBackend backend
                        => SQL.TableFieldsFulfillConstraint (BeamSqlBackendCanSerialize backend) UserTable
                        => Scrypt.Encrypted Password
                        -> User.Creatable 'Validation.Parsed
                        -> UserTable (Beam.QExpr backend s)
fromParsedCreatableUser encryptedPassword creatable = UserTable.Row { id                = SQL.def
                                                                    , username          = SQL.literal <| creatable ^. #username
                                                                    , emailAddress      = SQL.literal <| creatable ^. #emailAddress
                                                                    , encryptedPassword = SQL.literal encryptedPassword
                                                                    , createdAt         = SQL.def
                                                                    , updatedAt         = SQL.def
                                                                    }

toReadableUser :: UserTable.Row -> User.Readable
toReadableUser UserTable.Row {..} = User.Readable { id = coerce id, username = coerce username, emailAddress = coerce emailAddress }
