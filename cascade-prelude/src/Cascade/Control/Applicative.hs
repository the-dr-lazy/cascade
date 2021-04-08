{-|
Module      : Cascade.Control.Applicative
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Control.Applicative (pureMaybe, module Control.Applicative) where

import           Control.Applicative

pureMaybe :: Applicative f => Coercible (Maybe a) (maybe a) => a -> maybe a -> f a
pureMaybe def = pure . fromMaybe def . coerce
