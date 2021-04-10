{-|
Module      : Cascade.Data.Text.Finite
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cascade.Data.Text.Finite (Finite, pattern Finite, Error, un, mk, validate) where

import qualified Cascade.Data.Either                as Either
import qualified Data.Text                          as Text

newtype Finite (minimum :: Nat) (maximum :: Nat) = Mk { un :: Text }

type role Finite nominal nominal

pattern Finite :: Text -> Finite minimum maximum
pattern Finite a <- Mk a

data Error = Error
  { minimum :: Natural
  , maximum :: Natural
  , actual  :: Natural
  }

mk :: forall minimum maximum
    . KnownNat minimum
   => KnownNat maximum => CmpNat minimum maximum ~ 'LT => Text -> Either Error (Finite minimum maximum)
mk input = Mk input <$ validate minimum maximum input
 where
  minimum = natVal (Proxy @minimum)
  maximum = natVal (Proxy @maximum)

validate :: Natural -- ^ minimum
         -> Natural -- ^ maximum
         -> Text    -- ^ input
         -> Either Error ()
validate minimum maximum input = Either.leftUnless (minimum <= actual && actual <= maximum) Error { .. }
  where actual = fromIntegral . Text.length <| input
