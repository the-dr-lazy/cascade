module Cascade.Api.Data.Task
  ( Task
  , Id
  , Readable(..)
  , RawCreatable(..)
  , ParsedCreatable(..)
  , RawUpdatable(..)
  , ParsedUpdatable(..)
  , RawCreatableValidationErrors(..)
  , RawUpdatableValidationErrors(..)
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
import qualified Data.Text                     as Text
import           Chronos                        ( Time )
import           Validation

data Task

type Id = Data.Id Task

data Readable = Readable
  { id         :: Id
  , title      :: Text
  , deadlineAt :: FormattedOffsetDatetime
  , projectId  :: Project.Id
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data RawCreatable = RawCreatable
  { title      :: Text
  , deadlineAt :: FormattedOffsetDatetime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data ParsedCreatable = ParsedCreatable
  { title      :: Text
  , deadlineAt :: FormattedOffsetDatetime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data RawCreatableValidationErrors = TitleEmpty | DateNotFuture
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

parseRawCreatableTask
  :: RawCreatable
  -> Time
  -> Validation (NonEmpty RawCreatableValidationErrors) ParsedCreatable
parseRawCreatableTask RawCreatable {..} now =
  ParsedCreatable <$> validateTitle <*> validateDeadlineAt
 where
  validateTitle :: Validation (NonEmpty RawCreatableValidationErrors) Text
  validateTitle = title <$ failureIf (Text.null title) TitleEmpty

  validateDeadlineAt
    :: Validation
         (NonEmpty RawCreatableValidationErrors)
         FormattedOffsetDatetime
  validateDeadlineAt =
    deadlineAt <$ failureIf (isPast deadlineAt now) DateNotFuture


data RawUpdatable = RawUpdatable
  { title      :: Maybe Text
  , deadlineAt :: Maybe FormattedOffsetDatetime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data ParsedUpdatable = ParsedUpdatable
  { title      :: Maybe Text
  , deadlineAt :: Maybe FormattedOffsetDatetime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)


data RawUpdatableValidationErrors = TitleEmptyU | DateNotFutureU
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

parseRawUpdatableTask
  :: RawUpdatable
  -> Time
  -> Validation (NonEmpty RawUpdatableValidationErrors) ParsedUpdatable
parseRawUpdatableTask RawUpdatable {..} now =
  ParsedUpdatable <$> validateTitle <*> validateDeadlineAt
 where
  validateTitle
    :: Validation (NonEmpty RawUpdatableValidationErrors) (Maybe Text)
  validateTitle = case title of
    Just t  -> Just t <$ failureIf (Text.null t) TitleEmptyU
    Nothing -> Success Nothing

  validateDeadlineAt
    :: Validation
         (NonEmpty RawUpdatableValidationErrors)
         (Maybe FormattedOffsetDatetime)
  validateDeadlineAt = case deadlineAt of
    Just date -> Just date <$ failureIf (isPast date now) DateNotFutureU
    Nothing   -> Success Nothing
