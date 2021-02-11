module Cascade.Api.Data.OffsetDatetime
  ( FormattedOffsetDatetime(..)
  , isPast
  )
where

import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                , Value(..)
                                                )
import           Data.Aeson.Types               ( prependFailure
                                                , typeMismatch
                                                , JSONPathElement(..)
                                                , parserThrowError
                                                )
import           Chronos                        ( OffsetDatetime
                                                , Time
                                                , offsetDatetimeToTime
                                                , encode_YmdHMSz
                                                , hyphen
                                                , SubsecondPrecision(..)
                                                , OffsetFormat(..)
                                                , parser_YmdHMSz
                                                )

import           Data.Attoparsec.Text           ( parse
                                                , IResult(..)
                                                )

newtype FormattedOffsetDatetime = FormattedOffsetDatetime
  { unFormattedOffsetDatetime :: OffsetDatetime }
  deriving stock (Generic)
  deriving newtype (Show, Eq)

instance FromJSON FormattedOffsetDatetime where
  parseJSON (String x) =
    case parse (parser_YmdHMSz OffsetFormatColonAuto hyphen) x of
      Done _ r -> pure $ FormattedOffsetDatetime r
      _        -> parserThrowError [Key "deadlineAt"]
                                   "parsing deadlineAt failed, invalid date"

  parseJSON invalid = prependFailure "parsing deadlineAt failed, "
                                     (typeMismatch "String" invalid)


instance ToJSON FormattedOffsetDatetime where
  toJSON date = String $ encode_YmdHMSz OffsetFormatColonAuto
                                        SubsecondPrecisionAuto
                                        hyphen
                                        (unFormattedOffsetDatetime date)

isPast :: FormattedOffsetDatetime -> Time -> Bool
isPast date now = now > offsetDatetimeToTime (unFormattedOffsetDatetime date)
