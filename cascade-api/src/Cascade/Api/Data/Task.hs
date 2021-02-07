module Cascade.Api.Data.Task
  ( Task
  , Id
  , Readable(..)
  , Creatable(..)
  , Updatable(..)
  , FormattedOffsetDatetime(..)
  ) where

import qualified Cascade.Api.Data.Id           as Data
import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                )
import           Data.Generics.Labels           ( )
import qualified Cascade.Api.Data.Project      as Project
import           Chronos                        ( OffsetDatetime )

newtype FormattedOffsetDatetime = FormattedOffsetDatetime
  { unFormattedOffsetDatetime :: OffsetDatetime }
  deriving stock (Generic)
  deriving newtype (Show, Eq)

instance FromJSON FormattedOffsetDatetime where
  parseJSON = undefined

instance ToJSON FormattedOffsetDatetime where
  toJSON = undefined

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

data Creatable = Creatable
  { title      :: Text
  , deadlineAt :: FormattedOffsetDatetime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data Updatable = Updatable
  { title      :: Maybe Text
  , deadlineAt :: Maybe FormattedOffsetDatetime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)
