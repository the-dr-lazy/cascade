module Cascade.Api.Data.Task
  ( Task
  , Id
  , Readable(..)
  , RawCreatable(..)
  , ParsedCreatable(..)
  , Updatable(..)
  , RawCreatableValidationErrors(..)
  , parseRawCreatableTask
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


data Updatable = Updatable
  { title      :: Maybe Text
  , deadlineAt :: Maybe FormattedOffsetDatetime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)
