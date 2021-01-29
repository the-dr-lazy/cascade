module Cascade.Api.Data.Task
  ( Task
  , Id
  , Readable(..)
  , Creatable(..)
  , Updatable(..)
  ) where

import qualified Cascade.Api.Data.Id           as Data
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Generics.Labels           ( )
import qualified Cascade.Api.Data.Project      as Project
import           Chronos                        ( Time )

data Task

type Id = Data.Id Task

data Readable = Readable
  { id         :: Id
  , title      :: Text
  , deadlineAt :: Time
  , projectId  :: Project.Id
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data Creatable = Creatable
  { title      :: Text
  , deadlineAt :: Time
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data Updatable = Updatable
  { title      :: Maybe Text
  , deadlineAt :: Maybe Time
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)
