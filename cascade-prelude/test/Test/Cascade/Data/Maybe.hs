module Test.Cascade.Data.Maybe
    ( tests
    ) where

import qualified Cascade.Data.Maybe  as Maybe
import           Hedgehog
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import           Test.Tasty
import           Test.Tasty.Hedgehog
import qualified Validation

tests :: TestTree
tests = testGroup "Cascade.Data.Maybe" [toSuccessTests]

toSuccessTests :: TestTree
toSuccessTests = testGroup "toSuccess" [testProperty "output errors in nonEmpty" prop]
 where
  prop = property do
    x <- forAll $ Gen.int (Range.linear 0 10000)
    Maybe.toSuccess @_ @Any x Nothing === Validation.Failure (x :| [])
