{-|
Module      : Cascade.Data.Attoparsec.ByteString
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Data.Attoparsec.ByteString (parseMaybe, base64, module Data.Attoparsec.ByteString) where

import           Data.Attoparsec.ByteString
import           Data.ByteString.Base64              ( isBase64 )
import           Prelude                      hiding ( init
                                                     , takeWhile
                                                     )

parseMaybe :: Parser a -> ByteString -> Maybe a
parseMaybe parser = rightToMaybe . parseOnly (parser <* endOfInput)

base64 :: ByteString -> Parser ByteString
base64 input | isBase64 input = pure input
             | otherwise      = empty
