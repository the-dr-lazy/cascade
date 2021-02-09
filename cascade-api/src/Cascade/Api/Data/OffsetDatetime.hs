module Cascade.Api.Data.OffsetDatetime
  ( FormattedOffsetDatetime(..)
  )
where

import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                )
import           Chronos                        ( OffsetDatetime )

newtype FormattedOffsetDatetime = FormattedOffsetDatetime
  { unFormattedOffsetDatetime :: OffsetDatetime }
  deriving stock (Generic)
  deriving newtype (Show, Eq)

instance FromJSON FormattedOffsetDatetime where
  parseJSON = undefined

instance ToJSON FormattedOffsetDatetime where
  toJSON = undefined
