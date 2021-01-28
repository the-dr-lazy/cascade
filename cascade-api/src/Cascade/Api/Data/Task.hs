module Cascade.Api.Data.Task
  ( TaskId(..)
  , Task
  , Readable(..)
  , Creatable(..)
  , Updatable(..)
  ) where

import           Cascade.Api.Data.Prelude
import           Control.Lens.TH                ( makeWrapped )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Generics.Labels           ( )
import           Servant.API                    ( FromHttpApiData
                                                , ToHttpApiData
                                                )
import qualified Cascade.Api.Data.Project as Project
import           Chronos                        ( Time )


newtype TaskId = TaskId
  { unId :: UUID }
  deriving stock Generic
  deriving newtype (Show, Eq, Ord, FromHttpApiData, ToHttpApiData, FromJSON, ToJSON)

makeWrapped ''TaskId

data Task

data instance Readable Task = TaskR
  { id         :: TaskId
  , title      :: Text
  , deadlineAt :: Time
  , projectId  :: Project.Id
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data instance Creatable Task = TaskC
  { title      :: Text
  , deadlineAt :: Time
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data instance Updatable Task = TaskU
  { title      :: Maybe Text
  , deadlineAt :: Maybe Time
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)
