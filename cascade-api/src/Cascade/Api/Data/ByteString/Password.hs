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

import           Cascade.Data.Validation
import qualified Cascade.Data.Validation            as Validation
import           Control.Selective                   ( ifS )
import           Data.Aeson                          ( ToJSON )
import           Data.Data                           ( Data )
import qualified Data.Text                          as Text
import qualified Polysemy

newtype Password = Mk
  { un :: ByteString }
  deriving newtype (Show, Eq)

pattern Password :: ByteString -> Password
pattern Password a <- Mk a
{-# COMPLETE Password #-}

data ValidationError
  = IsEmpty
  | IsShort
  deriving stock (Generic, Data, Show)
  deriving ToJSON via (ApiErrorFormat ValidationError)

instance Validation.ToMessage ValidationError where
  toMessage = \case
    IsEmpty -> "can't be empty"
    IsShort -> "should have at least 8 characters"

type ValidationErrors = NonEmpty ValidationError

mk :: Text -> Validation ValidationErrors Password
mk = Polysemy.run . validate @Password

instance Validatable Password where
  type Raw Password = Text
  type Errors Password = ValidationErrors

  validate input = pure $ Mk (encodeUtf8 input) <$ ifS (pure $ Text.null input) (failure IsEmpty) (failureIf (Text.length input < 8) IsShort)
