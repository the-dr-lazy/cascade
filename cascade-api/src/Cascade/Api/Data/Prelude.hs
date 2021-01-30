module Cascade.Api.Data.Prelude
  ( Validate
  , Validatable
  ) where

-- brittany-disable-next-binding
data Validate (a :: Type)

type family Validatable (f :: Type -> Type) (a :: Type) (error :: Type) where
  Validatable Identity a _     = a
  Validatable Validate _ error = error
