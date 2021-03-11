{-|
Module      : Cascade.Api.Hedgehog.Gen.Text
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Hedgehog.Gen.Text
  ( username
  , usernameWithValidity
  , emailAddress
  , emailAddressWithValidity
  , password
  , passwordWithValidity
  , nonEmptyText
  , nonEmptyTextWithValidity
  ) where

import           Cascade.Api.Hedgehog.Gen.Prelude
import qualified Data.Text                          as Text
import           Hedgehog                            ( MonadGen )
import qualified Hedgehog.Gen                       as Gen
import qualified Hedgehog.Range                     as Range

nonEmptyText :: MonadGen g => Int -> Validity -> g Text
nonEmptyText upperLimit Valid   = Gen.text (Range.linear 1 upperLimit) Gen.alphaNum
nonEmptyText _          Invalid = pure ""

nonEmptyTextWithValidity :: MonadGen g => Int -> g (Validity, Text)
nonEmptyTextWithValidity upperLimit = do
  validity <- Gen.enumBounded
  (validity, ) <$> nonEmptyText upperLimit validity

username :: MonadGen g => Validity -> g Text
username Valid = Gen.text (Range.linear 8 20) $ Gen.choice [Gen.hexit, pure '_']
username Invalid =
  Gen.choice [Gen.text (Range.exponential 21 100) Gen.hexit, Gen.text (Range.linear 0 7) Gen.hexit, Gen.text (Range.linear 8 20) Gen.unicode]

usernameWithValidity :: MonadGen g => g (Validity, Text)
usernameWithValidity = do
  validity <- Gen.enumBounded
  (validity, ) <$> username validity

emailAddress :: MonadGen g => Validity -> g Text
emailAddress Valid = do
  recipient <- Gen.text (Range.linear 1 64) Gen.alphaNum
  domain    <- Gen.text (Range.linear 1 63) Gen.alphaNum
  tld       <- Gen.text (Range.linear 1 24) Gen.lower
  pure $ recipient <> "@" <> domain <> "." <> tld
emailAddress Invalid = Gen.filterT ((== 0) . Text.count "@") $ Gen.text (Range.exponential 0 256) Gen.unicode

emailAddressWithValidity :: MonadGen g => g (Validity, Text)
emailAddressWithValidity = do
  validity <- Gen.enumBounded
  (validity, ) <$> emailAddress validity

password :: MonadGen g => Validity -> g Text
password Valid   = Gen.text (Range.linear 8 64) Gen.unicodeAll
password Invalid = Gen.text (Range.linear 0 7) Gen.unicodeAll

passwordWithValidity :: MonadGen g => g (Validity, Text)
passwordWithValidity = do
  validity <- Gen.enumBounded
  (validity, ) <$> password validity
