{-|
Module      : Cascade.Api.Data.Aeson.FieldErrorFormat
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

{-# LANGUAGE UndecidableInstances #-}

module Cascade.Api.Data.Aeson.FieldErrorFormat
    ( FieldErrorFormat (..)
    ) where

import           Data.Aeson   ( FromJSON, ToJSON )
import qualified Data.Aeson   as Aeson
import           GHC.Generics ( Rep )

newtype FieldErrorFormat (error :: Type)
  = FieldErrorFormat error

options :: Aeson.Options
options = Aeson.defaultOptions { Aeson.tagSingleConstructors = True, Aeson.omitNothingFields = True }

instance (Generic error, Aeson.GToJSON Aeson.Zero (Rep error)) => ToJSON (FieldErrorFormat error) where
  toJSON (FieldErrorFormat e) = Aeson.genericToJSON options e

instance (Generic error, Aeson.GFromJSON Aeson.Zero (Rep error)) => FromJSON (FieldErrorFormat error) where
  parseJSON = fmap FieldErrorFormat . Aeson.genericParseJSON options
