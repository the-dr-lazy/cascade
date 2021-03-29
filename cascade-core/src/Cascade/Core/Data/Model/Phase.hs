{-|
Module      : Cascade.Core.Data.Phase
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Core.Data.Model.Phase (Phase(..), SomePhase(..), Suitable) where

data Phase = Unknown | New | Persisted

data SomePhase (a :: Phase -> Type) where
  SomeNew       ::a 'New       -> SomePhase a
  SomePersisted ::a 'Persisted -> SomePhase a

type family Suitable (phase :: Phase) (f :: Phase -> Type) :: Type where
  Suitable 'New       f = SomePhase f
  Suitable 'Persisted f = f 'Persisted
