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

module Cascade.Data.Maybe (pureMaybe, module Data.Maybe) where

import           Data.Maybe

pureMaybe :: Applicative f => Coercible (Maybe a) (maybe a) => a -> maybe a -> f a
pureMaybe def = pure . fromMaybe def . coerce
