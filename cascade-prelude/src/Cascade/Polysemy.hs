{-|
Module      : Cascade.Polysemy
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cascade.Polysemy (constraint) where

import           Polysemy
import           Unsafe.Coerce

constraint :: Members r r' => Sem r a -> Sem r' a
constraint = unsafeCoerce
