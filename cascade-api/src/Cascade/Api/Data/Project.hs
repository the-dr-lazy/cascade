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

module Cascade.Api.Data.Project
  ( Project
  , Id
  , Readable(..)
  , Creatable(..)
  , Updatable(..)
  ) where

import           Cascade.Api.Data.Prelude
                                         hiding ( Id )
import qualified Cascade.Api.Data.Prelude      as Prelude
import           Control.Lens.TH                ( makeWrapped )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Generics.Labels           ( )
import           Servant.API                    ( FromHttpApiData
                                                , ToHttpApiData
                                                )

data Project

type Id = Prelude.Id Project

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
