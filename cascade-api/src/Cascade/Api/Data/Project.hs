{-|
Module      : Cascade.Api.Data.Project
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Data.Project (Project, Id, Readable(..), Creatable(..), Updatable(..)) where

import qualified Cascade.Api.Data.Id                as Data
import           Data.Aeson                          ( FromJSON
                                                     , ToJSON
                                                     )
import           Data.Generics.Labels                ( )

data Project

type Id = Data.Id Project

data Readable = Readable
  { id   :: Id
  , name :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data Creatable = Creatable
  { name :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data Updatable = Updatable
  { name :: Maybe Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)
