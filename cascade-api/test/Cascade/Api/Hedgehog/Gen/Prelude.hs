{-|
Module      : Cascade.Api.Hedgehog.Gen.Prelude
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Hedgehog.Gen.Prelude (Validity(..)) where

data Validity
  = Valid
  | Invalid
  deriving stock (Show, Eq, Enum, Bounded)
