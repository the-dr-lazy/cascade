{-|
Module      : Cascade.Log.Severity
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Log.Severity (Severity(..)) where

data Severity
  = Debug
  | Info
  | Warning
  | Error
  | Panic
  deriving stock Eq