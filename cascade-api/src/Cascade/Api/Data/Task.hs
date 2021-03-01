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
import           Data.Aeson                          ( FromJSON(..)
                                                     , ToJSON(..)
                                                     )
import qualified Cascade.Api.Data.Project           as Project
import           Cascade.Api.Data.OffsetDatetime     ( FormattedOffsetDatetime
                                                     , unFormattedOffsetDatetime
                                                     )
import           Cascade.Api.Data.OffsetDatetime.Deadline
                                                     ( Deadline )
import qualified Cascade.Api.Data.OffsetDatetime.Deadline
                                                    as Deadline
import qualified Cascade.Data.Text                  as Text
import qualified Cascade.Data.Text.NonEmpty         as Text.NonEmpty
import           Chronos                             ( Time )
import           Cascade.Api.Data.Prelude
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

data RawCreatableV f = RawCreatable
  { title      :: Validatable f Text (Maybe Text.NonEmpty.ValidationErrors)
  , deadlineAt :: Validatable f FormattedOffsetDatetime (Maybe Deadline.ValidationErrors)
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
  validateTitle = Text.NonEmpty.mk title |> first \e -> mempty { title = Just e } :: RawCreatableValidationErrors

  validateDeadlineAt :: Validation RawCreatableValidationErrors Deadline
  validateDeadlineAt =
    Deadline.mk (unFormattedOffsetDatetime deadlineAt) now |> first \e -> mempty { deadlineAt = Just e } :: RawCreatableValidationErrors

data RawUpdatableV f = RawUpdatable
  { title      :: Validatable f (Maybe Text) (Maybe Text.NonEmpty.ValidationErrors)
  , deadlineAt :: Validatable f (Maybe FormattedOffsetDatetime) (Maybe Deadline.ValidationErrors)
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
    Just t  -> Just <$> Text.NonEmpty.mk t |> first \e -> mempty { title = Just e } :: RawUpdatableValidationErrors
    Nothing -> Success Nothing

  validateDeadlineAt :: Validation RawUpdatableValidationErrors (Maybe Deadline)
  validateDeadlineAt = case deadlineAt of
    Just date ->
      Just <$> Deadline.mk (unFormattedOffsetDatetime date) now |> first \e -> mempty { deadlineAt = Just e } :: RawUpdatableValidationErrors
    Nothing -> Success Nothing
