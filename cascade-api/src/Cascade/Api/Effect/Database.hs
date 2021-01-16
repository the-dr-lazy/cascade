{-# LANGUAGE NoImplicitPrelude #-}

module Cascade.Api.Effect.Database
  ( DatabaseL
  , TableFieldsFulfillConstraint
  , runSelectReturningList
  , runSelectReturningOne
  , runInsertReturningOne
  , runUpdateReturningOne
  , runDeleteReturningOne
  , runPostgres
  , all
  , lookup
  , update
  , insert
  , delete
  ) where

import           Cascade.Api.Database           ( Database
                                                , database
                                                )
import           Control.Lens                   ( Getter
                                                , (^.)
                                                )
import           Data.Functor.Identity
import           Data.Kind
import           Database.Beam                  ( Beamable
                                                , PrimaryKey
                                                )
import qualified Database.Beam                 as Beam
import           Database.Beam.Backend          ( BeamSqlBackend )
import qualified Database.Beam.Backend.SQL.BeamExtensions
                                               as Beam
                                                ( runDeleteReturningList
                                                , runInsertReturningList
                                                , runUpdateReturningList
                                                )
import qualified Database.Beam.Postgres        as Beam
                                                ( Postgres )
import           Database.Beam.Postgres         ( Pg
                                                , runBeamPostgres
                                                )
import qualified Database.Beam.Query.Internal  as Beam
import           Database.Beam.Schema.Tables    ( HasConstraint(..) )
import qualified Database.PostgreSQL.Simple    as Postgres
import           GHC.Generics
import           Polysemy                       ( Embed
                                                , Member
                                                , Members
                                                , Sem
                                                , embed
                                                , interpret
                                                , makeSem
                                                )
import           Prelude                        ( ($)
                                                , (.)
                                                , Bool
                                                , IO
                                                , Maybe
                                                , Text
                                                , fmap
                                                , listToMaybe
                                                , (|>)
                                                )

data DatabaseL backend (m :: Type -> Type) a where
  RunSelectReturningList ::(BeamSqlBackend backend, Beam.FromBackendRow backend a) => Beam.SqlSelect backend a -> DatabaseL backend m [a]
  RunSelectReturningOne ::(BeamSqlBackend backend, Beam.FromBackendRow backend a) => Beam.SqlSelect backend a -> DatabaseL backend m (Maybe a)
  RunInsertReturningOne ::(BeamSqlBackend backend, Beamable table, Beam.FromBackendRow backend (table Identity))=> Beam.SqlInsert backend table -> DatabaseL backend m (Maybe (table Identity))
  RunUpdateReturningOne ::(BeamSqlBackend backend, Beamable table, Beam.FromBackendRow backend (table Identity)) => Beam.SqlUpdate backend table -> DatabaseL backend m (Maybe (table Identity))
  RunDeleteReturningOne ::(BeamSqlBackend backend, Beamable table, Beam.FromBackendRow backend (table Identity)) => Beam.SqlDelete backend table -> DatabaseL backend m (Maybe (table Identity))

makeSem ''DatabaseL

runPostgres :: Member (Embed IO) r
            => (forall b . (Postgres.Connection -> IO b) -> IO b)
            -> Sem (DatabaseL Beam.Postgres ': r) a
            -> Sem r a
runPostgres withConnection = interpret \case
  RunSelectReturningList sql -> Beam.runSelectReturningList sql |> runSql
  RunSelectReturningOne  sql -> Beam.runSelectReturningOne sql |> runSql
  RunInsertReturningOne sql ->
    Beam.runInsertReturningList sql |> runSql |> fmap listToMaybe
  RunUpdateReturningOne sql ->
    Beam.runUpdateReturningList sql |> runSql |> fmap listToMaybe
  RunDeleteReturningOne sql ->
    Beam.runDeleteReturningList sql |> runSql |> fmap listToMaybe
 where
  runSql :: Member (Embed IO) r => Pg a -> Sem r a
  runSql sql = embed $ withConnection (`runBeamPostgres` sql)

type DatabaseEntityGetter backend table
  = Getter
      (Beam.DatabaseSettings backend Database)
      (Beam.DatabaseEntity backend Database (Beam.TableEntity table))

all :: forall backend table s
     . BeamSqlBackend backend
    => DatabaseEntityGetter backend table
    -> Beam.Q backend Database s (table (Beam.QExpr backend s))
all = Beam.all_ . (database ^.)

lookup :: forall table backend
        . Beam.Table table
       => BeamSqlBackend backend
       => Beam.HasQBuilder backend
       => Beam.SqlValableTable backend (PrimaryKey table)
       => Beam.HasTableEquality backend (PrimaryKey table)
       => Getter
            (Beam.DatabaseSettings backend Database)
            ( Beam.DatabaseEntity
                backend
                Database
                (Beam.TableEntity table)
            )
       -> PrimaryKey table Identity
       -> Beam.SqlSelect backend (table Identity)
lookup getter = Beam.lookup_ $ database ^. getter

insert :: forall backend table s
        . ( BeamSqlBackend backend
          , Beam.ProjectibleWithPredicate
              Beam.AnyType
              ()
              Text
              (table (Beam.QField s))
          )
       => Getter
            (Beam.DatabaseSettings backend Database)
            (Beam.DatabaseEntity backend Database (Beam.TableEntity table))
       -> Beam.SqlInsertValues backend (table (Beam.QExpr backend s))
       -> Beam.SqlInsert backend table
insert getter = Beam.insert $ database ^. getter

update :: forall backend table
        . (BeamSqlBackend backend, Beamable table)
       => DatabaseEntityGetter backend table
       -> (forall s . table (Beam.QField s) -> Beam.QAssignment backend s)
       -> (  forall s
           . table (Beam.QExpr backend s)
          -> Beam.QExpr backend s Bool
          )
       -> Beam.SqlUpdate backend table
update getter = Beam.update $ database ^. getter

delete :: forall backend table
        . BeamSqlBackend backend
       => DatabaseEntityGetter backend table
       -> (  forall s
           . (forall s' . table (Beam.QExpr backend s'))
          -> Beam.QExpr backend s Bool
          )
       -> Beam.SqlDelete backend table
delete getter = Beam.delete $ database ^. getter

-- brittany-disable-next-binding
type family TableFieldsFulfillConstraint' (table :: Type -> Type) :: Constraint where
  TableFieldsFulfillConstraint' U1 = ()
  TableFieldsFulfillConstraint' (x :*: y) = (TableFieldsFulfillConstraint' x, TableFieldsFulfillConstraint' y)
  TableFieldsFulfillConstraint' (K1 R (HasConstraint constraint x)) = (constraint x)
  TableFieldsFulfillConstraint' (M1 _ _ table) = TableFieldsFulfillConstraint' table

-- brittany-disable-next-binding
type TableFieldsFulfillConstraint (constraint :: Type -> Constraint) (table :: (Type -> Type) -> Type)
  = (Generic (table (HasConstraint constraint)), TableFieldsFulfillConstraint' (Rep (table (HasConstraint constraint))))
