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

module Cascade.Api.Data.ByteString.Password (Password, pattern Password, ValidationError(..), ValidationErrors, mk, un) where

import           Control.Selective                   ( ifS )
import           Data.Aeson                          ( FromJSON
                                                     , ToJSON
                                                     )
import qualified Data.Text                          as Text
import           Validation

newtype Password = Mk
  { un :: ByteString }
  deriving newtype (Show, Eq)

pattern Password :: ByteString -> Password
pattern Password a <- Mk a
{-# COMPLETE Password #-}

data ValidationError
  = IsEmpty
  | IsShort
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type ValidationErrors = NonEmpty ValidationError

mk :: Text -> Validation ValidationErrors Password
mk input = Mk (encodeUtf8 input) <$ validate input

validate :: Text -> Validation ValidationErrors ()
validate input = ifS (pure $ Text.null input) (failure IsEmpty) (failureIf (Text.length input < 8) IsShort)
