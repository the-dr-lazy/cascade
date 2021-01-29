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
  ( Id(..)
  , Project
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

newtype Id = Id
  { unId :: UUID }
  deriving stock Generic
  deriving newtype (Show, Eq, Ord, FromHttpApiData, ToHttpApiData, FromJSON, ToJSON)

makeWrapped ''Id

data Project

data instance Readable Project = Readable
  { id   :: Id
  , name :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data instance Creatable Project = Creatable
  { name :: Text }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

data instance Updatable Project = Updatable
  { name :: Maybe Text }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)
