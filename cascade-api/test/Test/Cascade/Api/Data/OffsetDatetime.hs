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
