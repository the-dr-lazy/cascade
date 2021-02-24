{-|
Module      : Test.Cascade.Api.Data.OffsetDatetime
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Test.Cascade.Api.Data.OffsetDatetime where

import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Cascade.Api.Hedgehog.Gen.Chronos
                                               as Gen
import qualified Data.Aeson                    as Aeson
import           Cascade.Api.Data.OffsetDatetime
                                                ( FormattedOffsetDatetime(..)
                                                , unFormattedOffsetDatetime
                                                )

prop_aesonFormattedOffsetDatetime :: Property
prop_aesonFormattedOffsetDatetime = property $ do
  x <- forAll (FormattedOffsetDatetime <$> Gen.offsetDateTime)
  tripping x Aeson.encode Aeson.eitherDecode
