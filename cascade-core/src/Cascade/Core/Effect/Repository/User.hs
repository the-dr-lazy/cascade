{-|
Module      : Cascade.Core.Effect.Repository.User
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Core.Effect.Repository.User
  ( UserL
  , findByUsername
  , findByEmailAddress
  , doesExistsByUsername
  , doesExistsByEmailAddress
  , write
  , runDatabase
  ) where

import qualified Cascade.Core.Data.Model.Phase      as Phase
import           Cascade.Core.Data.Model.User        ( EmailAddress
                                                     , User
                                                     , Username
                                                     )
import qualified Cascade.Core.Data.Model.User       as User
import qualified Cascade.Core.Effect.Database       as Database
import           Cascade.Core.Effect.Database        ( DatabaseL )
import qualified Cascade.Core.Effect.Scrypt         as Scrypt
import           Cascade.Core.Effect.Scrypt          ( ScryptL )
import           Cascade.Core.Effect.Time            ( TimeL )
import qualified Cascade.Core.Internal.Data.Contract.Database.SQL
                                                    as SQL
import qualified Cascade.Core.Internal.Data.Contract.Database.SQL.Query
                                                    as SQL.Query
import qualified Cascade.Core.Internal.Data.Contract.Database.SQL.Query.User
                                                    as SQL.Query.User
import qualified Cascade.Core.Internal.Data.Contract.Database.UserTable
                                                    as UserTable
import           Cascade.Core.Internal.Data.Contract.Database.UserTable
                                                     ( UserTable )
import qualified Cascade.Core.Internal.Data.Model.User.EmailAddress
                                                    as EmailAddress
import qualified Cascade.Core.Internal.Data.Model.User.Username
                                                    as Username
import           Control.Lens                        ( (^.) )
import           Database.Beam                       ( insertExpressions
                                                     , insertValues
                                                     )
import qualified Database.Beam                      as Beam
import           Database.Beam.Backend               ( BeamSqlBackend
                                                     , BeamSqlBackendCanSerialize
                                                     , SqlNull
                                                     )
import           Polysemy
import           Polysemy.Error
import qualified Polysemy.Error                     as Error
import qualified Relude.Unsafe                      as Unsafe

data UserL m a where
  FindByUsername           ::Username 'Phase.Unknown -> UserL m (Either (Username 'Phase.New) (User 'Phase.Persisted))
  FindByEmailAddress       ::EmailAddress 'Phase.Unknown -> UserL m (Either (EmailAddress 'Phase.New) (User 'Phase.Persisted))
  DoesExistsByUsername     ::Username 'Phase.Unknown -> UserL m (Either (Username 'Phase.New) (Username 'Phase.Persisted))
  DoesExistsByEmailAddress ::EmailAddress 'Phase.Unknown -> UserL m (Either (EmailAddress 'Phase.New) (EmailAddress 'Phase.Persisted))
  Write                    ::User 'Phase.New -> UserL m (User 'Phase.Persisted)

makeSem ''UserL

runDatabase :: Beam.HasQBuilder backend
            => Beam.FromBackendRow backend Bool
            => SQL.TableFieldsFulfillConstraints
                 '[Beam.FromBackendRow backend , Beam.HasSqlEqualityCheck backend , BeamSqlBackendCanSerialize backend]
                 UserTable
            => Members '[Error UserTable.MalformationError , DatabaseL backend , TimeL] r => Sem (UserL ': r) a -> Sem r a
runDatabase = interpret \case
  FindByUsername username -> SQL.Query.User.byUsername username |> SQL.select |> Database.runSelectReturningOne |> chainedTo \case
    Nothing  -> pure . Left <| Username.unsafePhaseCoerce username
    Just row -> fmap Right . Error.fromEither <| UserTable.unsafePersistedUserFromRow row

  FindByEmailAddress emailAddress -> SQL.Query.User.byEmailAddress emailAddress |> SQL.select |> Database.runSelectReturningOne |> chainedTo
    \case
      Nothing  -> pure . Left . EmailAddress.unsafePhaseCoerce <| emailAddress
      Just row -> fmap Right . Error.fromEither <| UserTable.unsafePersistedUserFromRow row

  DoesExistsByUsername username -> do
    let sieve = SQL.filter <| #username `SQL.eq` SQL.literal (Username.un username)
    existance <- SQL.Query.existance #users sieve |> SQL.select |> Database.runSelectReturningOne |> fmap Unsafe.fromJust

    pure <| if existance then Right (Username.unsafePhaseCoerce username) else Left (Username.unsafePhaseCoerce username)

  DoesExistsByEmailAddress emailAddress -> do
    let sieve = SQL.filter <| #emailAddress `SQL.eq` SQL.literal (EmailAddress.un emailAddress)
    existance <- SQL.Query.existance #users sieve |> SQL.select |> Database.runSelectReturningOne |> fmap Unsafe.fromJust

    pure <| if existance then Right (EmailAddress.unsafePhaseCoerce emailAddress) else Left (EmailAddress.unsafePhaseCoerce emailAddress)

  Write user -> do
    row <- UserTable.newUserToRow user
    insertValues [row] |> SQL.insert #users |> Database.runInsert
    pure . User.unsafePhaseCoerce <| user
