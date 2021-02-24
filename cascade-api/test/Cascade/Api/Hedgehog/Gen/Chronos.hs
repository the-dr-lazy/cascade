{-|
Module      : Cascade.Api.Hedgehog.Gen.Chronos
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Hedgehog.Gen.Chronos
  ( time
  , offsetDateTime
  , past
  , future
  , deadline
  , deadlineWithValidity
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

future :: MonadGen g => g OffsetDatetime
future = do
  year   <- Gen.int (Range.constant 2040 2060)
  month  <- Gen.int (Range.constant 1 12)
  day    <- Gen.int (Range.constant 1 28)
  hour   <- Gen.int (Range.constant 0 23)
  minute <- Gen.int (Range.constant 0 59)
  second <- Gen.int (Range.constant 0 59)
  offset <- Offset <$> Gen.int (Range.constant 0 5)
  let t = timeFromYmdhms year month day hour minute second
  pure $ timeToOffsetDatetime offset t

past :: MonadGen g => g OffsetDatetime
past = do
  year   <- Gen.int (Range.constant 2000 2020)
  month  <- Gen.int (Range.constant 1 12)
  day    <- Gen.int (Range.constant 1 28)
  hour   <- Gen.int (Range.constant 0 23)
  minute <- Gen.int (Range.constant 0 59)
  second <- Gen.int (Range.constant 0 59)
  offset <- Offset <$> Gen.int (Range.constant 0 5)
  let t = timeFromYmdhms year month day hour minute second
  pure $ timeToOffsetDatetime offset t

deadline :: MonadGen g => Validity -> g OffsetDatetime
deadline Valid   = future
deadline Invalid = past

deadlineWithValidity :: MonadGen g => g (Validity, OffsetDatetime)
deadlineWithValidity = do
  validity <- Gen.enumBounded
  (validity, ) <$> deadline validity
