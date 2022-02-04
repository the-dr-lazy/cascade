{-|
Module      : Test.Cascade.Api.Data.OffsetDatetime
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2022 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Test.Cascade.Api.Data.OffsetDatetime
    ( tests
    ) where

import           Cascade.Api.Data.OffsetDatetime  ( FormattedOffsetDatetime (..) )
import qualified Cascade.Api.Hedgehog.Gen.Chronos as Gen
import qualified Data.Aeson                       as Aeson
import           Hedgehog
import           Test.Tasty
import           Test.Tasty.Hedgehog

tests :: TestTree
tests =
  testGroup "Test.Cascade.Api.Data.OffsetDatetime" [testProperty "Aeson instance for FormattedOffsetDatetime" prop_aesonFormattedOffsetDatetime]

prop_aesonFormattedOffsetDatetime :: Property
prop_aesonFormattedOffsetDatetime = property $ do
  x <- forAll (FormattedOffsetDatetime <$> Gen.offsetDateTime)
  tripping x Aeson.encode Aeson.eitherDecode
