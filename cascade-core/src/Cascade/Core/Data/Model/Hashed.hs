{-|
Module      : Cascade.Core.Data.Model.Hashed
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Core.Data.Model.Hashed (Hashed, pattern Hashed, un, mk) where

import qualified Crypto.Scrypt                      as Scrypt

newtype Hashed (a :: Type) = Mk
  { un :: ByteString }
  deriving newtype (Show, Eq, Ord)

pattern Hashed :: ByteString -> Hashed a
pattern Hashed a <- Mk a
{-# COMPLETE Hashed #-}

mk :: Coercible ByteString a => a -> IO (Hashed a)
mk = fmap Mk . coerce . Scrypt.encryptPassIO' . coerce
