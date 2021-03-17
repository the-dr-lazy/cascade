{-|
Module      : Cascade.Api.Data.OffsetDatetime.Deadline
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Data.OffsetDatetime.Deadline (Deadline, un, mk) where

import           Cascade.Data.Chronos.Future         ( Future )
import qualified Cascade.Data.Chronos.Future        as Future
import           Chronos                             ( OffsetDatetime
                                                     , Time
                                                     )
import           Data.Data                           ( Data )
import           Cascade.Api.Data.OffsetDatetime     ( FormattedOffsetDatetime
                                                     , unFormattedOffsetDatetime
                                                     )
import           Cascade.Data.Validation
import qualified Cascade.Data.Validation            as Validation
import           Data.Aeson                          ( FromJSON
                                                     , ToJSON
                                                     )
import           Cascade.Api.Effect.Time             ( TimeL )
import qualified Cascade.Api.Effect.Time            as Time

newtype Deadline = Deadline (Future OffsetDatetime)
  deriving stock Show

un :: Deadline -> OffsetDatetime
un (Deadline future) = Future.un future

mk :: OffsetDatetime -> Time -> Maybe Deadline
mk date now = Deadline <$> Future.mk date now

data ValidationError = IsPast
  deriving stock (Generic, Data, Show)
  deriving ToJSON via (ApiErrorFormat ValidationError)

instance Validation.ToMessage ValidationError where
  toMessage IsPast = "past date"

type ValidationErrors = NonEmpty ValidationError

instance Validatable OffsetDatetime Deadline where
  type Errors OffsetDatetime Deadline = ValidationErrors
  type Effects OffsetDatetime Deadline = '[TimeL]

  parse date = do
    now <- Time.now
    pure . maybeToSuccess (IsPast :| []) $ mk date now

instance Validatable FormattedOffsetDatetime Deadline where
  type Errors FormattedOffsetDatetime Deadline = ValidationErrors
  type Effects FormattedOffsetDatetime Deadline = '[TimeL]

  parse date = do
    now <- Time.now
    pure . maybeToSuccess (IsPast :| []) $ mk (unFormattedOffsetDatetime date) now
