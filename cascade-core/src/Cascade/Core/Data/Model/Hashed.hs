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

module Cascade.Core.Data.Model.Hashed (Hashed, pattern Hashed, un, mk, coMk) where

import qualified Cascade.Data.Attoparsec.ByteString as Atto.W8
import qualified Crypto.Scrypt                      as Scrypt
import           Data.Attoparsec.ByteString          ( (<?>) )
import qualified Data.Attoparsec.ByteString.Char8   as Atto.C8

newtype Hashed (a :: Type) = Mk { un :: ByteString }
  deriving stock Show
  deriving newtype (Eq, Ord)

pattern Hashed :: ByteString -> Hashed a
pattern Hashed a <- Mk a
{-# COMPLETE Hashed #-}

-- | Hash a @ByteString@.
-- FIXME: scrypt package doesn't maintain anymore, use cryptonite
mk :: Coercible ByteString a => a -> IO (Hashed a)
mk = fmap Mk . coerce . Scrypt.encryptPassIO' . coerce

-- | Parse a @ByteString@ for a preformed hash.
coMk :: ByteString -> Maybe (Hashed a)
coMk input = input |> Atto.W8.parseMaybe do
  Atto.C8.decimal @Word32 <?> "logN"
  Atto.C8.char separator
  Atto.C8.decimal @Word32 <?> "r"
  Atto.C8.char separator
  Atto.C8.decimal @Word32 <?> "p"
  Atto.C8.char separator
  Atto.W8.base64 =<< Atto.C8.takeWhile1 (/= separator) <?> "salt"
  Atto.C8.char separator
  Atto.W8.base64 =<< Atto.C8.takeByteString <?> "hash"
  pure <| Mk input
  where separator = '|'
