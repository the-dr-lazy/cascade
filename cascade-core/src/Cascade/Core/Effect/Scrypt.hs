{-|
Module      : Cascade.Core.Effect.Scrypt
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Core.Effect.Scrypt (ScryptL, hash, test, run) where

import           Cascade.Core.Data.Model             ( Hashed )
import qualified Cascade.Core.Data.Model.Hashed     as Hashed
import qualified Crypto.Scrypt                      as Scrypt
import           Polysemy                     hiding ( run )

data ScryptL (m :: Type -> Type) (a :: Type) where
  Hash ::Coercible ByteString b => b -> ScryptL m (Hashed b)

makeSem ''ScryptL

test :: Coercible ByteString a => a -> Hashed a -> Bool
test x (Hashed.un -> x') = Scrypt.verifyPass' (coerce x) (coerce x')

run :: Member (Embed IO) r => Sem (ScryptL ': r) a -> Sem r a
run = interpret \case
  Hash x -> embed <| Hashed.mk x
