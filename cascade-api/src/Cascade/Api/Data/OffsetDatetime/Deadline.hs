module Cascade.Api.Data.OffsetDatetime.Deadline
  ( Deadline(..)
  , ValidationError(..)
  , ValidationErrors
  , pattern Deadline
  , un
  , mk
  )
where

import           Cascade.Api.Data.OffsetDatetime
                                                ( FormattedOffsetDatetime, isPast )
import           Control.Lens.TH                ( makeWrapped )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Chronos                        ( Time )
import           Validation
import           Database.Beam.Postgres.Syntax  ( PgValueSyntax
                                                , defaultPgValueSyntax
                                                )
import qualified Database.Beam                 as Beam
import qualified Database.Beam.Backend         as Beam
import           Database.Beam.Postgres         ( Postgres
                                                , ResultError(..)
                                                )
import qualified Database.PostgreSQL.Simple.FromField
                                               as Postgres
import qualified Database.PostgreSQL.Simple.ToField
                                               as Postgres

newtype Deadline = Mk
  { un :: FormattedOffsetDatetime }
  deriving newtype (Show, Eq, FromJSON, ToJSON)

makeWrapped ''Deadline

pattern Deadline :: FormattedOffsetDatetime -> Deadline
pattern Deadline a <- Mk a
{-# COMPLETE Deadline #-}

data ValidationError
  = IsPast
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type ValidationErrors = NonEmpty ValidationError

mk :: FormattedOffsetDatetime -> Time -> Validation ValidationErrors Deadline
mk input now = Mk input <$ validate input now

validate :: FormattedOffsetDatetime -> Time -> Validation ValidationErrors ()
validate input now = failureIf (isPast input now) IsPast
