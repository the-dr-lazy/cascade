{-|
Module      : Cascade.Api.Data.Prelude
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Data.Prelude (Validate, Validatable) where

-- brittany-disable-next-binding
data Validate (a :: Type)

type family Validatable (f :: Type -> Type) (a :: Type) (error :: Type) where
  Validatable Identity a _     = a
  Validatable Validate _ error = error
