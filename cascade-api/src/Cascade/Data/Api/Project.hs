module Cascade.Data.Api.Project
  ( Id(..)
  , Project
  , Readable(..)
  , Creatable(..)
  , Updatable(..)
  ) where

import           Cascade.Data.Api.Prelude
import           Control.Lens.TH                ( makeWrapped )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Generics.Labels           ( )
import           Servant.API                    ( ToHttpApiData )

newtype Id = Id
  { unId :: UUID }
  deriving stock Generic
  deriving newtype (Show, Eq, Ord, ToHttpApiData, FromJSON, ToJSON)

makeWrapped ''Id

data Project

data instance Readable Project = ProjectR
  { id   :: Id
  , name :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data instance Creatable Project = ProjectC
  { name :: Text }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data instance Updatable Project = ProjectU
  { name :: Maybe Text }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)
