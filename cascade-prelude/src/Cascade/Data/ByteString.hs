{-|
Module      : Cascade.Data.ByteString
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

{-# LANGUAGE NoImplicitPrelude #-}

module Cascade.Data.ByteString (trim) where

import           Data.ByteString
import           Data.Word8                          ( isSpace )
import           Prelude                             ( (.)
                                                     , fst
                                                     )

trim :: ByteString -> ByteString
trim = fst . spanEnd isSpace . dropWhile isSpace
{-# INLINE trim #-}
