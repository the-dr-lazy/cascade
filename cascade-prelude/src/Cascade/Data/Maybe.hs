{-|
Module      : Cascade.Data.Maybe
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Data.Maybe
    ( module Data.Maybe
    , toSuccess
    ) where

import           Data.Maybe
import           Validation ( Validation )
import qualified Validation


toSuccess :: e -> Maybe a -> Validation (NonEmpty e) a
toSuccess e = Validation.maybeToSuccess (e :| [])
