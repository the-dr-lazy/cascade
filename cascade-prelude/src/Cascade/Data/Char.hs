module Cascade.Data.Char
  ( isAlphaNumUnderscore
  , module Data.Char
  ) where

import           Data.Char

isAlphaNumUnderscore :: Char -> Bool
isAlphaNumUnderscore c = isAlphaNum c || c == '_'
