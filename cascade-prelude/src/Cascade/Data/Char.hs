{-|
Module      : Cascade.Data.Char
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Data.Char (isAlphaNumUnderscore, module Data.Char) where

import           Data.Char

isAlphaNumUnderscore :: Char -> Bool
isAlphaNumUnderscore c = isAlphaNum c || c == '_'
