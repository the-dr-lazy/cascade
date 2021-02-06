module Cascade.Api.Hedgehog.Gen.Prelude
  ( Validity(..)
  ) where

data Validity
  = Valid
  | Invalid
  deriving stock (Show, Eq, Enum, Bounded)
