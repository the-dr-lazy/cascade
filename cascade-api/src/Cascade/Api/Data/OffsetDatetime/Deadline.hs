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

module Cascade.Api.Data.OffsetDatetime.Deadline
    ( Deadline
    , mk
    , parse
    , un
    ) where

import qualified Cascade.Api.Data.Aeson.FieldErrorFormat as Aeson
import           Cascade.Api.Data.OffsetDatetime         (FormattedOffsetDatetime)
import           Cascade.Data.Chronos.Future             (Future)
import qualified Cascade.Data.Chronos.Future             as Future
import           Cascade.Data.Validation                 (Validation)
import qualified Cascade.Data.Validation                 as Validation
import           Chronos                                 (OffsetDatetime, Time)
import           Data.Aeson                              (FromJSON, ToJSON)

newtype Deadline
  = Deadline (Future OffsetDatetime)
  deriving stock (Show)

un :: Deadline -> OffsetDatetime
un (Deadline future) = Future.un future

mk :: Time -> OffsetDatetime -> Maybe Deadline
mk now input = Deadline <$> Future.mk input now

data Error = IsPast deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via Aeson.FieldErrorFormat Error

type Errors = NonEmpty Error

type instance Validation.Errors OffsetDatetime Deadline = Errors
type instance Validation.Errors FormattedOffsetDatetime Deadline = Errors

parse :: Time -> OffsetDatetime -> Validation Errors Deadline
parse now = Validation.maybeToSuccess (IsPast :| []) . mk now
