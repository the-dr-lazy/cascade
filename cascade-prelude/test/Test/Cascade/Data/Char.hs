{-|
Module      : Test.Cascade.Data.Char
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Test.Cascade.Data.Char (tests) where

import           Cascade.Data.Char
import qualified Data.Char                          as Char
import           Hedgehog
import qualified Hedgehog.Gen                       as Gen
import           Test.Tasty
import           Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Cascade.Data.Char" [isAlphaNumUnderscoreTests]

isAlphaNumUnderscoreTests :: TestTree
isAlphaNumUnderscoreTests = testGroup
    "isAlphaNumUnderscore"
    [ testProperty "on non-(alphanumeric + underscore) input returns False" prop_false
    , testProperty "on alphanumeric + underscore input returns True"        prop_true
    ]
  where
    prop_false = property do
        char <- forAll $ Gen.filter ((/= '_') *> not . Char.isAlphaNum) Gen.unicodeAll
        isAlphaNumUnderscore char === False
    prop_true = property do
        char <- forAll $ Gen.choice [Gen.alphaNum, pure '_']
        isAlphaNumUnderscore char === True
