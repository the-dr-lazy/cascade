module Cascade.Api.Data.OffsetDatetime.Deadline
  ( Deadline
  , ValidationError(..)
  , ValidationErrors
  , un
  , mk
  )
where

import           Cascade.Data.Chronos.Future
import           Chronos                        ( OffsetDatetime )

type Deadline = Future OffsetDatetime
