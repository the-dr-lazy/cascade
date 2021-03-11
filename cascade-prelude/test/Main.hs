{-|
Module      : Main
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Main (main) where

import qualified Test.Cascade.Data.ByteString
import qualified Test.Cascade.Data.Char
import qualified Test.Cascade.Data.Chronos.Future
import qualified Test.Cascade.Data.Text.NonEmpty
import           Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Tests" tests

tests :: [TestTree]
tests =
  [ Test.Cascade.Data.Char.tests
  , Test.Cascade.Data.ByteString.tests
  , Test.Cascade.Data.Chronos.Future.tests
  , Test.Cascade.Data.Text.NonEmpty.tests
  ]
