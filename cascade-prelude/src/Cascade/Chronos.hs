{-|
Module      : Cascade.Chronos
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2022 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Chronos
    ( module Chronos
    , builderDby
    , builderDbyHMSz
    ) where

import           Chronos
import qualified Chronos.Locale.English     as Chronos
import qualified Data.Text.Lazy.Builder     as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Vector                as Vector

{- | Given a 'Datetime', constructs a 'Text' 'TB.Builder' corresponding to a
Day\/Month\/Year,Hour\/Minute\/Second\/Offset encoding of the given 'Datetime'.

Example: @29 Dec 2019 22:00:00.000 +00:00@
-}
builderDbyHMSz :: DatetimeFormat -> Datetime -> TB.Builder
builderDbyHMSz (DatetimeFormat dateSeparator separator timeSeparator) (Datetime date time) =
  builderDby dateSeparator date
    <> bSeparator
    <> Chronos.builder_HMS (Chronos.SubsecondPrecisionFixed 3) timeSeparator time
    <> bSeparator
    <> Chronos.builderOffset Chronos.OffsetFormatColonOn (Chronos.Offset 0)
 where
  bSeparator :: TB.Builder
  bSeparator = maybeToMonoid . fmap TB.singleton <| separator

builderDby :: Maybe Char -> Chronos.Date -> TB.Builder
builderDby (maybeToMonoid . fmap TB.singleton -> bSeparator) (Date (Year y) m d) =
  zeroPadDayOfMonth d <> bSeparator <> TB.fromText (Chronos.caseMonth Chronos.abbreviated m) <> bSeparator <> TB.decimal y

{- | Copy of the 'zeroPadDayOfMonth' from the choronos library which doesn't export it.

Reference: https://github.com/andrewthad/chronos/blob/aa6d2b0969c4c5216ff9e45da1574e194fafefc1/src/Chronos.hs#L2678-L2679
-}
zeroPadDayOfMonth :: Chronos.DayOfMonth -> TB.Builder
zeroPadDayOfMonth (DayOfMonth d) = if d < 100 then Vector.unsafeIndex twoDigitTextBuilders d else TB.decimal d

twoDigitTextBuilders :: Vector.Vector TB.Builder
twoDigitTextBuilders = Vector.fromList . map TB.fromText <| twoDigitTexts
{-# NOINLINE twoDigitTextBuilders #-}

twoDigitTexts :: [Text]
twoDigitTexts = [ show x <> show y | x <- [(0 :: Int) .. 9], y <- [(0 :: Int) .. 9] ]
