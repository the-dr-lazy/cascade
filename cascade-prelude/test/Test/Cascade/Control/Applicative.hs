module Test.Cascade.Control.Applicative (tests) where

import           Cascade.Control.Applicative
import           Hedgehog
import qualified Hedgehog.Gen                       as Gen
import qualified Hedgehog.Range                     as Range
import           Test.Tasty
import           Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Cascade.Control.Applicative" [pureMaybeTests]

pureMaybeTests :: TestTree
pureMaybeTests = testGroup "pureMaybe"
                           [testProperty "output given value when Just" prop_just, testProperty "output default when Nothing" prop_nothing]
 where
  prop_just = property do
    x <- forAll $ Gen.int (Range.linear 0 10000)
    y <- forAll $ Gen.int (Range.linear 0 10000)
    pureMaybe x (Last (Just y)) === Identity y
  prop_nothing = property do
    x <- forAll $ Gen.int (Range.linear 0 10000)
    pureMaybe x (Last Nothing) === Identity x
