{-|
Module      : Cascade.Core.Data.ByteString.Password
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Core.Data.Model.User.Password (Password, pattern Password, Error(..), Errors, mk, un) where

import           Control.Selective                   ( ifS )
import qualified Data.Text                          as Text
import           Validation                          ( Validation )
import qualified Validation

newtype Password = Mk { un :: ByteString }
  deriving newtype (Show, Eq)

pattern Password :: ByteString -> Password
pattern Password a <- Mk a
{-# COMPLETE Password #-}

data Error
  = IsEmpty
  | IsShort
  deriving stock (Generic, Show)

type Errors = NonEmpty Error

mk :: Text -> Validation Errors Password
mk input = Mk (encodeUtf8 input) <$ validate input

validate :: Text -> Validation Errors ()
validate input = ifS (pure $ Text.null input) (Validation.failure IsEmpty) (Validation.failureIf (Text.length input < 8) IsShort)
