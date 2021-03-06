{-|
Module      : Cascade.Api.Effect.Scrypt
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Effect.Scrypt
    ( Encrypted
    , ScryptL
    , encryptPassword
    , pattern Encrypted
    , run
    , un
    , verifyPassword
    ) where

import           Cascade.Api.Data.ByteString.Password ( Password, pattern Password )
import qualified Cascade.Api.Data.ByteString.Password as Password
import           Control.Lens.TH                      ( makeWrapped )
import qualified Crypto.Scrypt                        as Scrypt
import           Polysemy
    ( Embed, Member, Sem, embed, interpret, makeSem )

newtype Encrypted (a :: Type)
  = Mk { un :: ByteString }
  deriving newtype (Eq, Ord, Show)

makeWrapped ''Encrypted

pattern Encrypted :: ByteString -> Encrypted a
pattern Encrypted a <- Mk a
{-# COMPLETE Encrypted #-}

data ScryptL (m :: Type -> Type) (a :: Type) where Encrypt :: ByteString -> ScryptL m (Encrypted b)

makeSem ''ScryptL

encryptPassword :: Member ScryptL r => Password -> Sem r (Encrypted Password)
encryptPassword = encrypt . Password.un

verifyPassword :: Password -> Encrypted Password -> Bool
verifyPassword (Password p) ep = Scrypt.verifyPass' (coerce p) (coerce ep)

run :: Member (Embed IO) r => Sem (ScryptL ': r) a -> Sem r a
run = interpret \case
  Encrypt x -> Scrypt.encryptPassIO' (coerce x) |> coerce |> fmap Mk |> embed
