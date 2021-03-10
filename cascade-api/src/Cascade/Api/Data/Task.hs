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

{-# LANGUAGE UndecidableInstances #-}
module Cascade.Api.Data.Task
  ( Task
  , Id
  , Readable(..)
  , Creatable(..)
  , Updatable(..)
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
import           Cascade.Api.Data.OffsetDatetime     ( FormattedOffsetDatetime() )
import           Cascade.Api.Data.OffsetDatetime.Deadline
                                                     ( Deadline )
import           Cascade.Data.Validation
import qualified Cascade.Data.Validation            as Validation
import qualified Cascade.Data.Text                  as Text
import           Polysemy                            ( Sem
                                                     , Members
                                                     )
import           Cascade.Polysemy                    ( constraint )

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

data Creatable v = Creatable
  { title      :: Validate v Text Text.NonEmpty
  , deadlineAt :: Validate v FormattedOffsetDatetime Deadline
  }
  deriving stock Generic

deriving via (Generically (Creatable 'Parsed)) instance Validatable (Creatable 'Raw) (Creatable 'Parsed)

deriving stock instance Show (Creatable 'Raw)
deriving stock instance Eq (Creatable 'Raw)
deriving anyclass instance ToJSON (Creatable 'Raw)
deriving anyclass instance FromJSON (Creatable 'Raw)

type RawCreatableValidationErrors = (Validation.Errors (Creatable 'Raw) (Creatable 'Parsed))

parseRawCreatableTask :: Members (Effects (Creatable 'Raw) (Creatable 'Parsed)) r
                      => Creatable 'Raw
                      -> Sem r (Validation (Errors (Creatable 'Raw) (Creatable 'Parsed)) (Creatable 'Parsed))
parseRawCreatableTask = constraint . validate

data Updatable v = Updatable
  { title      :: Validate v (Maybe Text) (Maybe Text.NonEmpty)
  , deadlineAt :: Validate v (Maybe FormattedOffsetDatetime) (Maybe Deadline)
  }
  deriving stock Generic

deriving via (Generically (Updatable 'Parsed)) instance Validatable (Updatable 'Raw) (Updatable 'Parsed)

deriving stock instance Show (Updatable 'Raw)
deriving stock instance Eq (Updatable 'Raw)
deriving anyclass instance ToJSON (Updatable 'Raw)
deriving anyclass instance FromJSON (Updatable 'Raw)

type RawUpdatableValidationErrors = (Validation.Errors (Updatable 'Raw) (Updatable 'Parsed))

parseRawUpdatableTask :: Members (Effects (Creatable 'Raw) (Creatable 'Parsed)) r
                      => Updatable 'Raw
                      -> Sem r (Validation (Errors (Updatable 'Raw) (Updatable 'Parsed)) (Updatable 'Parsed))
parseRawUpdatableTask = constraint . validate
