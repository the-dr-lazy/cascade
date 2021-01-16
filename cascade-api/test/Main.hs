module Main
  ( main
  ) where

import qualified Test.Cascade.Api.Network.Server.Api.Projects
import           Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Tests" tests

tests :: [TestTree]
tests = [Test.Cascade.Api.Network.Server.Api.Projects.tests]
