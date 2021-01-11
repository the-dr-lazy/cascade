module Cascade.Data.Api.Project
  ( Id(..)
  , Project
  , Readable(..)
  , Creatable(..)
  , Updatable(..)
  ) where

import           Cascade.Data.Api.Prelude
import           Data.Generics.Labels           ( )

newtype Id = Id
  { unId :: UUID }
  deriving stock Generic
  deriving newtype (Show, Eq)

data Project

data instance Readable Project = ProjectR
  { id   :: Id
  , name :: Text
  }
  deriving stock (Generic, Show, Eq)

newtype instance Creatable Project = ProjectC
  { name :: Text }
  deriving stock (Generic, Show, Eq)

newtype instance Updatable Project = ProjectU
  { name :: Maybe Text }
  deriving stock (Generic, Show, Eq)
