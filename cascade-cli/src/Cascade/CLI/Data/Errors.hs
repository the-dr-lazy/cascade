{-|
Module      : Cascade.CLI.Data.Errors
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.CLI.Data.Errors (Errors, Error(..), validateEmptyField) where

import           Validation                          ( Validation )
import qualified Validation

data Error = BusyHttpPortError | EmptyField
  deriving stock (Show, Eq)

type Errors = NonEmpty Error

validateEmptyField :: Last a -> Validation Errors a
validateEmptyField a = Validation.maybeToSuccess (EmptyField :| []) (getLast a)
