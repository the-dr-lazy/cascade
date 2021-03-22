{-|
Module      : Cascade.Api.Database.Sql
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Cascade.Api.Database.Sql
  ( (==)
  , Q
  , DatabaseEntityGetting
  , TableFieldsFulfillConstraint
  , TableFieldsFulfillConstraints
  , and
  , def
  , delete
  , eq
  , filter
  , filterProjectsByRelatedUsers
  , insert
  , literal
  , lookup
  , manyToMany
  , or
  , update
  , Beam.select
  ) where

import           Cascade.Api.Data.WrappedC           ( WrappedC )
import           Cascade.Api.Database
import           Cascade.Api.Database.ProjectTable   ( ProjectTable )
import           Cascade.Api.Database.UserTable      ( UserTable )
import           Control.Lens                 hiding ( (|>) )
import           Database.Beam                hiding ( Database
                                                     , ManyToMany
                                                     , Q
                                                     , delete
                                                     , insert
                                                     , update
                                                     )
import qualified Database.Beam                      as Beam
import           Database.Beam.Backend               ( BeamSqlBackend )
import           Database.Beam.Schema.Tables         ( HasConstraint )
import           GHC.Generics
import           Prelude                      hiding ( (==)
                                                     , all
                                                     , and
                                                     , filter
                                                     , or
                                                     )

type DatabaseEntityGetting backend table
  = Getting
      (DatabaseEntity backend Database (TableEntity table))
      (DatabaseSettings backend Database)
      (DatabaseEntity backend Database (TableEntity table))

type Q backend s a = Beam.Q backend Database s a

lookup :: _
       => Getter (DatabaseSettings backend Database) (DatabaseEntity backend Database (TableEntity table))
       -> PrimaryKey table Identity
       -> SqlSelect backend (table Identity)
lookup optic = Beam.lookup_ $ database ^. optic

insert :: _
       => Getter (DatabaseSettings backend Database) (DatabaseEntity backend Database (TableEntity table))
       -> SqlInsertValues backend (table (Beam.QExpr backend s))
       -> SqlInsert backend table
insert optic = Beam.insert $ database ^. optic

update :: _
       => DatabaseEntityGetting backend table
       -> (forall s . table (QField s) -> QAssignment backend s)
       -> (forall s . table (Beam.QExpr backend s) -> Beam.QExpr backend s Bool)
       -> SqlUpdate backend table
update optic = Beam.update $ database ^. optic

delete :: _
       => DatabaseEntityGetting backend table
       -> (forall s . (forall s' . table (Beam.QExpr backend s')) -> Beam.QExpr backend s Bool)
       -> SqlDelete backend table
delete optic = Beam.delete $ database ^. optic

(==) :: SqlEq expression a => a -> a -> expression Bool
(==) = (Beam.==.)

eq :: SqlEq expression a => Getting a (table expression) a -> a -> table expression -> expression Bool
optic `eq` x = \row -> view optic row == x

literal :: SqlValable (Beam.QGenExpr context backend s b) => Coercible a b => a -> Beam.QGenExpr context backend s b
literal = Beam.val_ . coerce

def :: _ => Beam.QGenExpr context backend s a
def = Beam.default_

filter :: _ => (a -> Beam.QExpr backend s Bool) -> Q backend s a -> Q backend s a
filter = Beam.filter_

and :: Applicative f
    => BeamSqlBackend backend
    => f (Beam.QGenExpr context backend s Bool)
    -> f (Beam.QGenExpr context backend s Bool)
    -> f (Beam.QGenExpr context backend s Bool)
and = liftA2 (Beam.&&.)

or :: Applicative f
   => BeamSqlBackend backend
   => f (Beam.QGenExpr context backend s Bool)
   -> f (Beam.QGenExpr context backend s Bool)
   -> f (Beam.QGenExpr context backend s Bool)
or = liftA2 (Beam.||.)

type ManyToMany backend s through left right
  =  Table through
  => Table left
  => Table right
  => BeamSqlBackend backend
  => SqlEq (Beam.QExpr backend s) (PrimaryKey left (Beam.QExpr backend s))
  => SqlEq (Beam.QExpr backend s) (PrimaryKey right (Beam.QExpr backend s))
  => DatabaseEntityGetting backend through
  -> Getting
       (PrimaryKey left (Beam.QExpr backend s))
       (through (Beam.QExpr backend s))
       (PrimaryKey left (Beam.QExpr backend s))
  -> Getting
       (PrimaryKey right (Beam.QExpr backend s))
       (through (Beam.QExpr backend s))
       (PrimaryKey right (Beam.QExpr backend s))
  -> Q backend s (left (Beam.QExpr backend s))
  -> Q backend s (right (Beam.QExpr backend s))
  -> Q backend s (left (Beam.QExpr backend s), right (Beam.QExpr backend s))

manyToMany :: ManyToMany backend s through left right
manyToMany through left right = Beam.manyToMany_ (database ^. through) (view left) (view right)

userProjectsRelationship :: Beam.ManyToMany backend Database UserTable ProjectTable
userProjectsRelationship = manyToMany #userProjects #userId #projectId

filterProjectsByRelatedUsers :: _
                             => Q backend s (UserTable (Beam.QExpr backend s))
                             -> Q backend s (ProjectTable (Beam.QExpr backend s))
                             -> Q backend s (ProjectTable (Beam.QExpr backend s))
filterProjectsByRelatedUsers queryUsers queryProjects = view _2 <$> userProjectsRelationship queryUsers queryProjects


-- brittany-disable-next-binding
type family TableFieldsFulfillConstraint' (constraint :: Type -> Constraint) (table :: Type -> Type) :: Constraint where
  TableFieldsFulfillConstraint' _ U1 = ()
  TableFieldsFulfillConstraint' constraint (x :*: y) = (TableFieldsFulfillConstraint' constraint x, TableFieldsFulfillConstraint' constraint y)
  TableFieldsFulfillConstraint' _ (K1 R (HasConstraint constraint (WrappedC x))) = (constraint (Unwrapped x), constraint (WrappedC x))
  TableFieldsFulfillConstraint' _ (K1 R (HasConstraint constraint x)) = (constraint x)
  TableFieldsFulfillConstraint' constraint (K1 R (PrimaryKey table _)) = TableFieldsFulfillConstraint constraint table
  TableFieldsFulfillConstraint' constraint (M1 _ _ table) = TableFieldsFulfillConstraint' constraint table

-- brittany-disable-next-binding
type TableFieldsFulfillConstraint (constraint :: Type -> Constraint) (table :: (Type -> Type) -> Type)
  = (Generic (table (HasConstraint constraint)), TableFieldsFulfillConstraint' constraint (Rep (table (HasConstraint constraint))))

-- brittany-disable-next-binding
type family TableFieldsFulfillConstraints (constraint :: [Type -> Constraint]) (table :: (Type -> Type) -> Type) :: Constraint where
  TableFieldsFulfillConstraints '[] _ = ()
  TableFieldsFulfillConstraints (constraint ': constraints) table = (TableFieldsFulfillConstraint constraint table, TableFieldsFulfillConstraints constraints table)
