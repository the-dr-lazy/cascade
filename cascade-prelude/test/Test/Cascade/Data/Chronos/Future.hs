{-|
Module      : Test.Cascade.Data.Chronos.Future
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Test.Cascade.Data.Chronos.Future
    ( tests
    ) where

import qualified Cascade.Data.Chronos.Future as Future
import qualified Chronos
import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range
import           Test.Tasty
import           Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Cascade.Data.Chronos.Future" [futureTests]

futureTests :: TestTree
futureTests = testGroup "mk" [testProperty "date in future is valid" prop_future, testProperty "date in past is invalid" prop_past]
 where
  prop_future = property do
    now          <- evalIO Chronos.now
    randomNumber <- forAll $ Gen.int64 (Range.linear 0 10000)
    let x = Chronos.Time $ Chronos.getTime now + 1 + randomNumber
    Future.mk x now |> evalMaybe |> void
  prop_past = property do
    now          <- evalIO Chronos.now
    randomNumber <- forAll $ Gen.int64 (Range.linear 0 10000)
    let x = Chronos.Time $ Chronos.getTime now - 1 - randomNumber
    Future.mk x now === Nothing
