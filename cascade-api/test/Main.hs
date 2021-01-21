module Main
  ( main
  ) where

import qualified Test.Cascade.Api.StateMachine
import           Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Tests" tests

tests :: [TestTree]
tests = [Test.Cascade.Api.StateMachine.tests]
