module Cascade.Api.Data.OffsetDatetime
  ( FormattedOffsetDatetime(..)
  , isPast
  )
where

import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                )
import           Chronos                        ( OffsetDatetime, Time, offsetDatetimeToTime )

newtype FormattedOffsetDatetime = FormattedOffsetDatetime
  { unFormattedOffsetDatetime :: OffsetDatetime }
  deriving stock (Generic)
  deriving newtype (Show, Eq)

instance FromJSON FormattedOffsetDatetime where
  parseJSON = undefined

instance ToJSON FormattedOffsetDatetime where
  toJSON = undefined

isPast :: FormattedOffsetDatetime -> Time -> Bool
isPast date now = now > offsetDatetimeToTime (unFormattedOffsetDatetime date)
