module Cascade.Api.Hedgehog.Gen.Chronos
  ( time
  , offsetDateTime
  )
where

import           Prelude                 hiding ( second )
import           Cascade.Api.Hedgehog.Gen.Prelude
import           Chronos                        ( Time
                                                , Offset(..)
                                                , OffsetDatetime
                                                , timeFromYmdhms
                                                , timeToOffsetDatetime
                                                )
import           Hedgehog                       ( MonadGen(GenBase) )
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range


time :: MonadGen g => g Time
time = do
  year   <- Gen.int (Range.constant 2000 2040)
  month  <- Gen.int (Range.constant 1 12)
  day    <- Gen.int (Range.constant 1 28)
  hour   <- Gen.int (Range.constant 0 23)
  minute <- Gen.int (Range.constant 0 59)
  second <- Gen.int (Range.constant 0 59)
  pure $ timeFromYmdhms year month day hour minute second

offsetDateTime :: MonadGen g => g OffsetDatetime
offsetDateTime = do
  offset <- Offset <$> Gen.int (Range.constant 0 5)
  timeToOffsetDatetime offset <$> time
