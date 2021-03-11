{-|
Module      : Cascade.Api.Data.Task
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Data.Task
  ( Task
  , Id
  , Readable(..)
  , RawCreatableV(..)
  , RawCreatable
  , ParsedCreatable(..)
  , RawUpdatableV(..)
  , RawUpdatable
  , ParsedUpdatable(..)
  , RawCreatableValidationErrors
  , RawUpdatableValidationErrors
  , parseRawCreatableTask
  , parseRawUpdatableTask
  ) where

import qualified Cascade.Api.Data.Id                as Data
import           Cascade.Api.Data.OffsetDatetime     ( FormattedOffsetDatetime
                                                     , unFormattedOffsetDatetime
                                                     )
import           Cascade.Api.Data.OffsetDatetime.Deadline
                                                     ( Deadline )
import qualified Cascade.Api.Data.OffsetDatetime.Deadline
                                                    as Deadline
import           Cascade.Api.Data.Prelude
import qualified Cascade.Api.Data.Project           as Project
import qualified Cascade.Data.Text                  as Text
import qualified Cascade.Data.Text.NonEmpty         as Text.NonEmpty
import           Chronos                             ( Time )
import           Data.Aeson                          ( FromJSON(..)
                                                     , ToJSON(..)
                                                     )
import           Data.Generics.Labels                ( )
import           Data.Monoid.Generic
import           Validation

data Task

type Id = Data.Id Task

data Readable = Readable
  { id         :: Id
  , title      :: Text.NonEmpty
  , deadlineAt :: FormattedOffsetDatetime
  , projectId  :: Project.Id
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data TitleValidationError = IsEmpty
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type TitleValidationErrors = NonEmpty TitleValidationError

data DeadlineValidationError = IsPast
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type DeadlineValidationErrors = NonEmpty DeadlineValidationError

data RawCreatableV f = RawCreatable
  { title      :: Validatable f Text (Maybe TitleValidationErrors)
  , deadlineAt :: Validatable f FormattedOffsetDatetime (Maybe DeadlineValidationErrors)
  }
  deriving stock Generic

type RawCreatable = RawCreatableV Identity

deriving stock instance Show RawCreatable
deriving stock instance Eq RawCreatable
deriving anyclass instance FromJSON RawCreatable
deriving anyclass instance ToJSON RawCreatable

type RawCreatableValidationErrors = RawCreatableV Validate

deriving stock instance Show RawCreatableValidationErrors
deriving anyclass instance FromJSON RawCreatableValidationErrors
deriving anyclass instance ToJSON RawCreatableValidationErrors
deriving via (GenericSemigroup RawCreatableValidationErrors) instance Semigroup RawCreatableValidationErrors
deriving via (GenericMonoid RawCreatableValidationErrors) instance Monoid RawCreatableValidationErrors

data ParsedCreatable = ParsedCreatable
  { title      :: Text.NonEmpty
  , deadlineAt :: Deadline
  }
  deriving stock (Generic, Show, Eq)

parseRawCreatableTask :: RawCreatable -> Time -> Validation RawCreatableValidationErrors ParsedCreatable
parseRawCreatableTask RawCreatable {..} now = ParsedCreatable <$> validateTitle <*> validateDeadlineAt
 where
  validateTitle :: Validation RawCreatableValidationErrors Text.NonEmpty
  validateTitle = case Text.NonEmpty.mk title of
    Nothing -> Failure (mempty { title = Just (IsEmpty :| []) } :: RawCreatableValidationErrors)
    Just a  -> Success a

  validateDeadlineAt :: Validation RawCreatableValidationErrors Deadline
  validateDeadlineAt = case Deadline.mk (unFormattedOffsetDatetime deadlineAt) now of
    Nothing -> Failure (mempty { deadlineAt = Just (IsPast :| []) } :: RawCreatableValidationErrors)
    Just a  -> Success a

data RawUpdatableV f = RawUpdatable
  { title      :: Validatable f (Maybe Text) (Maybe TitleValidationErrors)
  , deadlineAt :: Validatable f (Maybe FormattedOffsetDatetime) (Maybe DeadlineValidationErrors)
  }
  deriving stock Generic

type RawUpdatable = RawUpdatableV Identity

deriving stock instance Show RawUpdatable
deriving stock instance Eq RawUpdatable
deriving anyclass instance FromJSON RawUpdatable
deriving anyclass instance ToJSON RawUpdatable

type RawUpdatableValidationErrors = RawUpdatableV Validate

deriving stock instance Show RawUpdatableValidationErrors
deriving anyclass instance FromJSON RawUpdatableValidationErrors
deriving anyclass instance ToJSON RawUpdatableValidationErrors
deriving via (GenericSemigroup RawUpdatableValidationErrors) instance Semigroup RawUpdatableValidationErrors
deriving via (GenericMonoid RawUpdatableValidationErrors) instance Monoid RawUpdatableValidationErrors

data ParsedUpdatable = ParsedUpdatable
  { title      :: Maybe Text.NonEmpty
  , deadlineAt :: Maybe Deadline
  }
  deriving stock (Generic, Show, Eq)

parseRawUpdatableTask :: RawUpdatable -> Time -> Validation RawUpdatableValidationErrors ParsedUpdatable
parseRawUpdatableTask RawUpdatable {..} now = ParsedUpdatable <$> validateTitle <*> validateDeadlineAt
 where
  validateTitle :: Validation RawUpdatableValidationErrors (Maybe Text.NonEmpty)
  validateTitle = case title of
    Just t -> Just <$> case Text.NonEmpty.mk t of
      Nothing -> Failure (mempty { title = Just (IsEmpty :| []) } :: RawUpdatableValidationErrors)
      Just a  -> Success a
    Nothing -> Success Nothing

  validateDeadlineAt :: Validation RawUpdatableValidationErrors (Maybe Deadline)
  validateDeadlineAt = case deadlineAt of
    Just date -> Just <$> case Deadline.mk (unFormattedOffsetDatetime date) now of
      Nothing -> Failure (mempty { deadlineAt = Just (IsPast :| []) } :: RawUpdatableValidationErrors)
      Just a  -> Success a
    Nothing -> Success Nothing
