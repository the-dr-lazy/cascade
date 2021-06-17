{-|
Module      : Cascade.Api.Data.ByteString.Password
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Data.ByteString.Password
    ( Error (..)
    , Errors
    , Password
    , mk
    , pattern Password
    , un
    ) where

import qualified Cascade.Api.Data.Aeson.FieldErrorFormat as Aeson
import           Cascade.Data.Validation                 (Validation)
import qualified Cascade.Data.Validation                 as Validation
import           Control.Selective                       (ifS)
import           Data.Aeson                              (FromJSON, ToJSON)
import qualified Data.Text                               as Text

newtype Password
  = Mk { un :: ByteString }
  deriving newtype (Eq, Show)

pattern Password :: ByteString -> Password
pattern Password a <- Mk a
{-# COMPLETE Password #-}

data Error = IsEmpty | IsShort deriving stock (Generic, Show)
  deriving (ToJSON) via Aeson.FieldErrorFormat Error
  deriving (FromJSON) via Aeson.FieldErrorFormat Error

type Errors = NonEmpty Error

type instance Validation.Errors Text Password = Errors

mk :: Text -> Validation Errors Password
mk input = Mk (encodeUtf8 input) <$ validate input

validate :: Text -> Validation Errors ()
validate input = ifS (pure $ Text.null input) (Validation.failure IsEmpty) (Validation.failureIf (Text.length input < 8) IsShort)
