{-|
Module      : Cascade.Api.Query
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

{-# LANGUAGE UndecidableInstances #-}
module Cascade.Api.Query
  ( Q
  , TableFieldsFulfillConstraint
  , TableFieldsFulfillConstraints
  , all
  , lookup
  , update
  , insert
  , delete
  , manyToMany
  , filterProjectsByRelatedUsers
  ) where

import qualified Cascade.Api.Data.Project      as Project
import qualified Cascade.Api.Data.User         as User
import           Cascade.Api.Data.WrappedC
import           Cascade.Api.Database
import           Cascade.Api.Database.ProjectTable
                                                ( ProjectTable )
import           Cascade.Api.Database.UserTable ( UserTable )
import           Control.Lens
import           Database.Beam           hiding ( Database
                                                , ManyToMany
                                                , Q
                                                , delete
                                                , insert
                                                , update
                                                )
import qualified Database.Beam                 as Beam
import           Database.Beam.Backend
import           Database.Beam.Query.Internal
                                         hiding ( Q )
import           Database.Beam.Schema.Tables    ( HasConstraint )
import           GHC.Generics
import           Prelude                 hiding ( all )

type DatabaseEntityGetter backend table
  = Getter
      (DatabaseSettings backend Database)
      (DatabaseEntity backend Database (TableEntity table))

type Q backend s a = Beam.Q backend Database s a

all :: forall backend table s
     . BeamSqlBackend backend
    => DatabaseEntityGetter backend table
    -> Q backend s (table (QExpr backend s))
all = all_ . (database ^.)

lookup :: forall table backend
        . Table table
       => BeamSqlBackend backend
       => HasQBuilder backend
       => SqlValableTable backend (PrimaryKey table)
       => HasTableEquality backend (PrimaryKey table)
       => Getter
            (DatabaseSettings backend Database)
            (DatabaseEntity backend Database (TableEntity table))
       -> PrimaryKey table Identity
       -> SqlSelect backend (table Identity)
lookup getter = lookup_ $ database ^. getter

insert :: forall backend table s
        . ( BeamSqlBackend backend
          , ProjectibleWithPredicate AnyType () Text (table (QField s))
          )
       => Getter
            (DatabaseSettings backend Database)
            (DatabaseEntity backend Database (TableEntity table))
       -> SqlInsertValues backend (table (QExpr backend s))
       -> SqlInsert backend table
insert getter = Beam.insert $ database ^. getter

update :: forall backend table
        . (BeamSqlBackend backend, Beamable table)
       => DatabaseEntityGetter backend table
       -> (forall s . table (QField s) -> QAssignment backend s)
       -> (forall s . table (QExpr backend s) -> QExpr backend s Bool)
       -> SqlUpdate backend table
update getter = Beam.update $ database ^. getter

delete :: forall backend table
        . BeamSqlBackend backend
       => DatabaseEntityGetter backend table
       -> (  forall s
           . (forall s' . table (QExpr backend s'))
          -> QExpr backend s Bool
          )
       -> SqlDelete backend table
delete getter = Beam.delete $ database ^. getter

type ManyToMany backend s through left right
  =  Table through
  => Table left
  => Table right
  => BeamSqlBackend backend
  => SqlEq (QExpr backend s) (PrimaryKey left (QExpr backend s))
  => SqlEq (QExpr backend s) (PrimaryKey right (QExpr backend s))
  => DatabaseEntityGetter backend through
  -> Getter
       (through (QExpr backend s))
       (PrimaryKey left (QExpr backend s))
  -> Getter
       (through (QExpr backend s))
       (PrimaryKey right (QExpr backend s))
  -> Q backend s (left (QExpr backend s))
  -> Q backend s (right (QExpr backend s))
  -> Q
       backend
       s
       ( left (QExpr backend s)
       , right (QExpr backend s)
       )

manyToMany :: ManyToMany backend s through left right
manyToMany through left right =
  manyToMany_ (database ^. through) (view left) (view right)

userProjectsRelationship :: Beam.ManyToMany
                              backend
                              Database
                              UserTable
                              ProjectTable
userProjectsRelationship = manyToMany #userProjects #userId #projectId

type FilterProjectsByUsers backend s
  =  HasSqlEqualityCheck backend (WrappedC User.Id)
  => HasSqlEqualityCheck backend (WrappedC Project.Id)
  => Q backend s (UserTable (QExpr backend s))
  -> Q backend s (ProjectTable (QExpr backend s))
  -> Q backend s (ProjectTable (QExpr backend s))

filterProjectsByRelatedUsers :: FilterProjectsByUsers backend s
filterProjectsByRelatedUsers queryUsers queryProjects =
  view _2 <$> userProjectsRelationship queryUsers queryProjects

-- brittany-disable-next-binding
type family TableFieldsFulfillConstraint' (table :: Type -> Type) :: Constraint where
  TableFieldsFulfillConstraint' U1 = ()
  TableFieldsFulfillConstraint' (x :*: y) = (TableFieldsFulfillConstraint' x, TableFieldsFulfillConstraint' y)
  TableFieldsFulfillConstraint' (K1 R (HasConstraint constraint (WrappedC x))) = (constraint (Unwrapped x), constraint (WrappedC x))
  TableFieldsFulfillConstraint' (K1 R (HasConstraint constraint x)) = (constraint x)
  TableFieldsFulfillConstraint' (M1 _ _ table) = TableFieldsFulfillConstraint' table

-- brittany-disable-next-binding
type TableFieldsFulfillConstraint (constraint :: Type -> Constraint) (table :: (Type -> Type) -> Type)
  = (Generic (table (HasConstraint constraint)), TableFieldsFulfillConstraint' (Rep (table (HasConstraint constraint))))

-- brittany-disable-next-binding
type family TableFieldsFulfillConstraints (constraint :: [Type -> Constraint]) (table :: (Type -> Type) -> Type) :: Constraint where
  TableFieldsFulfillConstraints '[] _ = ()
  TableFieldsFulfillConstraints (constraint ': constraints) table = (TableFieldsFulfillConstraint constraint table, TableFieldsFulfillConstraints constraints table)
