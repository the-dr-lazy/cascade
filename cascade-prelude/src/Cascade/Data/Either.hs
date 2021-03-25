{-|
Module      : Cascade.Data.Either
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Data.Either (leftUnless) where

leftUnless :: Bool -> e -> Either e ()
leftUnless p e | not p     = Left e
               | otherwise = Right ()
