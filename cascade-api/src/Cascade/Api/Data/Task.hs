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
  )
where

import qualified Cascade.Api.Data.Id           as Data
import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                )
import           Data.Generics.Labels           ( )
import qualified Cascade.Api.Data.Project      as Project
import           Cascade.Api.Data.OffsetDatetime
                                                ( FormattedOffsetDatetime
                                                , isPast
                                                )
import           Cascade.Api.Data.OffsetDatetime.Deadline
                                                ( Deadline )
import qualified Cascade.Api.Data.OffsetDatetime.Deadline
                                               as Deadline
import           Cascade.Api.Data.Text.NonEmpty ( NonEmptyText )
import qualified Cascade.Api.Data.Text.NonEmpty
                                               as Text.NonEmpty
import qualified Data.Text                     as Text
import           Chronos                        ( Time )
import           Cascade.Api.Data.Prelude
import           Data.Generics.Labels           ( )
import           Data.Monoid.Generic
import           Validation

data Task

type Id = Data.Id Task

data Readable = Readable
  { id         :: Id
  , title      :: NonEmptyText
  , deadlineAt :: Deadline
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
  { title      :: NonEmptyText
  , deadlineAt :: Deadline
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

parseRawCreatableTask
  :: RawCreatable
  -> Time
  -> Validation RawCreatableValidationErrors ParsedCreatable
parseRawCreatableTask RawCreatable {..} now =
  ParsedCreatable <$> validateTitle <*> validateDeadlineAt
 where
  validateTitle :: Validation RawCreatableValidationErrors NonEmptyText
  validateTitle = Text.NonEmpty.mk title
    |> first \e -> mempty { title = Just e } :: RawCreatableValidationErrors

  validateDeadlineAt :: Validation RawCreatableValidationErrors Deadline
  validateDeadlineAt = Deadline.mk deadlineAt now |> first \e ->
    mempty { deadlineAt = Just e } :: RawCreatableValidationErrors

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
  { title      :: Maybe NonEmptyText
  , deadlineAt :: Maybe Deadline
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

parseRawUpdatableTask
  :: RawUpdatable
  -> Time
  -> Validation RawUpdatableValidationErrors ParsedUpdatable
parseRawUpdatableTask RawUpdatable {..} now =
  ParsedUpdatable <$> validateTitle <*> validateDeadlineAt
 where
  validateTitle :: Validation RawUpdatableValidationErrors (Maybe NonEmptyText)
  validateTitle = case title of
    Just t -> Just <$> Text.NonEmpty.mk t |> first \e ->
      mempty { title = Just e } :: RawUpdatableValidationErrors
    Nothing -> Success Nothing

  validateDeadlineAt :: Validation RawUpdatableValidationErrors (Maybe Deadline)
  validateDeadlineAt = case deadlineAt of
    Just date -> Just <$> Deadline.mk date now |> first \e ->
      mempty { deadlineAt = Just e } :: RawUpdatableValidationErrors
    Nothing -> Success Nothing
