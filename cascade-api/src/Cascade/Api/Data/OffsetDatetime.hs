module Cascade.Api.Data.OffsetDatetime
  ( FormattedOffsetDatetime(..)
  )
where

import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                , Value(..)
                                                )
import           Data.Aeson.Types               ( prependFailure
                                                , typeMismatch
                                                , parserThrowError
                                                )
import qualified Chronos
import           Chronos.Types
import           Data.Attoparsec.Text           ( parseOnly
                                                , endOfInput
                                                )

newtype FormattedOffsetDatetime = FormattedOffsetDatetime
  { unFormattedOffsetDatetime :: OffsetDatetime }
  deriving stock Generic
  deriving newtype (Eq, Show)

instance FromJSON FormattedOffsetDatetime where
  parseJSON (String x) =
    let parser = Chronos.parser_YmdHMSz OffsetFormatColonAuto Chronos.hyphen
    in  case parseOnly (parser <* endOfInput) x of
          Right r -> pure $ FormattedOffsetDatetime r
          Left  e -> parserThrowError [] ("parsing date failed: " ++ e)

  parseJSON invalid =
    prependFailure "parsing date failed, " (typeMismatch "String" invalid)

instance ToJSON FormattedOffsetDatetime where
  toJSON date = String $ Chronos.encode_YmdHMSz
    OffsetFormatColonAuto
    SubsecondPrecisionAuto
    Chronos.hyphen
    (unFormattedOffsetDatetime date)
