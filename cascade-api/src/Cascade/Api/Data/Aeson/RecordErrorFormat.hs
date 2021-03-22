{-|
Module      : Cascade.Api.Data.Aeson.RecordErrorFormat
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

{-# LANGUAGE UndecidableInstances #-}

module Cascade.Api.Data.Aeson.RecordErrorFormat (RecordErrorFormat(..)) where

import qualified Data.Aeson                         as Aeson
import           Data.Aeson                          ( FromJSON
                                                     , ToJSON
                                                     )
import           GHC.Generics                        ( Rep )

newtype RecordErrorFormat (error :: Type) = RecordErrorFormat error

options :: Aeson.Options
options = Aeson.defaultOptions { Aeson.omitNothingFields = True }

instance (Generic error, Aeson.GToJSON Aeson.Zero (Rep error)) => ToJSON (RecordErrorFormat error) where
  toJSON (RecordErrorFormat e) = Aeson.genericToJSON options e

instance (Generic error, Aeson.GFromJSON Aeson.Zero (Rep error)) => FromJSON (RecordErrorFormat error) where
  parseJSON = fmap RecordErrorFormat . Aeson.genericParseJSON options
