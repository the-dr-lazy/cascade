{-|
Module      : Test.Cascade.Data.Text.NonEmpty
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Test.Cascade.Data.Text.NonEmpty
    ( tests
    ) where

import qualified Cascade.Data.Text.NonEmpty as Text.NonEmpty
import           Hedgehog
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Cascade.Data.Text.NonEmpty" [nonEmptyTests]

nonEmptyTests :: TestTree
nonEmptyTests = testGroup "mk" [testProperty "non empty text is valid" mkValid, testCase "empty text is invalid" mkInvalid]
 where
  mkValid = property do
    text <- forAll $ Gen.text (Range.linear 1 36) Gen.alphaNum
    Text.NonEmpty.mk text |> evalMaybe |> void
  mkInvalid = Text.NonEmpty.mk "" @?= Nothing
