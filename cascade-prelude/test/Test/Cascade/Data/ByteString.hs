{-|
Module      : Test.Cascade.Data.ByteString
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Test.Cascade.Data.ByteString
  ( tests
  ) where

import           Cascade.Data.ByteString
import qualified Data.ByteString               as W8
import qualified Data.Word8                    as Word8
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Test.Tasty
import           Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Cascade.Data.ByteString" [trimTests]

trimTests :: TestTree
trimTests = testGroup
  "trim"
  [ testProperty "idempotency" prop_idempotency
  -- , testProperty "trim = reverse . trim . reverse"
  , testProperty "no leading and traint space(s) in output"
                 prop_noLeadingAndTrailingSpace
  ]
 where
  prop_idempotency = property do
    i <- forAll $ Gen.bytes (Range.linear 8 64)
    trim (trim i) === trim i
  prop_noLeadingAndTrailingSpace = property do
    i <- forAll do
      leading  <- Gen.utf8 (Range.linear 0 8) $ pure ' '
      middle   <- Gen.utf8 (Range.linear 0 64) Gen.unicodeAll
      trailing <- Gen.utf8 (Range.linear 0 8) $ pure ' '
      pure $ leading <> middle <> trailing
    let o = trim i
    W8.takeWhile Word8.isSpace o === ""
    W8.takeWhile Word8.isSpace (W8.reverse o) === ""
