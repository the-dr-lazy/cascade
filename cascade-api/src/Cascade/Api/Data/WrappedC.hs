{-|
Module      : Cascade.Api.Data.WrappedC
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

{-# LANGUAGE UndecidableInstances #-}

module Cascade.Api.Data.WrappedC (WrappedC(..), C) where

import           Control.Lens                        ( Unwrapped
                                                     , Wrapped
                                                     , _Wrapped'
                                                     , review
                                                     , view
                                                     )
import qualified Database.Beam                      as Beam
import           Database.Beam.Backend               ( BackendFromField
                                                     , BeamSqlBackend
                                                     )
import qualified Database.Beam.Backend              as Beam
import qualified Database.PostgreSQL.Simple.FromField
                                                    as Postgres
                                                     ( FromField(fromField) )

newtype WrappedC a = WrappedC
  { unWrappedC :: a }
  deriving stock Generic
  deriving newtype (Show, Eq, Ord)

instance Wrapped a => Wrapped (WrappedC a)

instance ( Wrapped a
         , Typeable a
         , BeamSqlBackend backend
         , BackendFromField backend (WrappedC a)
         , Beam.FromBackendRow backend (Unwrapped a)
         ) =>
         Beam.FromBackendRow backend (WrappedC a)

instance ( Wrapped a
         , BeamSqlBackend backend
         , Beam.HasSqlEqualityCheck backend (Unwrapped a)
         ) =>
         Beam.HasSqlEqualityCheck backend (WrappedC a)

instance (Wrapped a, Postgres.FromField (Unwrapped a)) => Postgres.FromField (WrappedC a) where
    fromField a b = review (_Wrapped' . _Wrapped') <$> Postgres.fromField a b

instance (Wrapped a, Beam.HasSqlValueSyntax backend (Unwrapped a)) =>
         Beam.HasSqlValueSyntax backend (WrappedC a) where
    sqlValueSyntax = Beam.sqlValueSyntax . view (_Wrapped' . _Wrapped')

type family C (f :: Type -> Type) (a :: Type) :: Type where
  C f x = Beam.C f (WrappedC x)
