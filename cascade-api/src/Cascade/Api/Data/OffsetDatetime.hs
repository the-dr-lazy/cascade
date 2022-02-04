{-|
Module      : Cascade.Api.Data.OffsetDatetime
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2022 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Data.OffsetDatetime
    ( FormattedOffsetDatetime (..)
    ) where

import qualified Chronos
import           Chronos.Types
import           Data.Aeson           ( FromJSON (..), ToJSON (..), Value (..) )
import           Data.Aeson.Types     ( parserThrowError, prependFailure, typeMismatch )
import           Data.Attoparsec.Text ( endOfInput, parseOnly )

newtype FormattedOffsetDatetime
  = FormattedOffsetDatetime { unFormattedOffsetDatetime :: OffsetDatetime }
  deriving stock (Generic, Show)
  deriving newtype (Eq)

instance FromJSON FormattedOffsetDatetime where
  parseJSON (String x) =
    let parser = Chronos.parser_YmdHMSz OffsetFormatColonAuto Chronos.hyphen
    in  case parseOnly (parser <* endOfInput) x of
          Right r -> pure $ FormattedOffsetDatetime r
          Left  e -> parserThrowError [] ("parsing date failed: " ++ e)

  parseJSON invalid = prependFailure "parsing date failed, " (typeMismatch "String" invalid)

instance ToJSON FormattedOffsetDatetime where
  toJSON date = String $ Chronos.encode_YmdHMSz OffsetFormatColonAuto SubsecondPrecisionAuto Chronos.hyphen (unFormattedOffsetDatetime date)
