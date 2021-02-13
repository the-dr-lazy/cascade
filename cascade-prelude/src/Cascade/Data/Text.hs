{-# LANGUAGE NoImplicitPrelude #-}

module Cascade.Data.Text
  ( module Data.Text
  , NonEmpty
  , pattern NonEmpty
  , unNonEmpty
  , mkNonEmpty
  , NonEmptyValidationError(..)
  , NonEmptyValidationErrors
  )
where

import           Prelude                 hiding ( NonEmpty )
import qualified Prelude                        ( NonEmpty )
import           Data.Text
import           Control.Lens.TH                ( makeWrapped )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import qualified Data.Text                     as Text
import           Validation

newtype NonEmpty = Mk
  { unNonEmpty :: Text }
  deriving newtype (Show, Eq, FromJSON, ToJSON)

makeWrapped ''NonEmpty

pattern NonEmpty :: Text -> NonEmpty
pattern NonEmpty a <- Mk a
{-# COMPLETE NonEmpty #-}

data NonEmptyValidationError
  = IsEmpty
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type NonEmptyValidationErrors = Prelude.NonEmpty NonEmptyValidationError

mkNonEmpty :: Text -> Validation NonEmptyValidationErrors NonEmpty
mkNonEmpty input = Mk input <$ validateNonEmpty input

validateNonEmpty :: Text -> Validation NonEmptyValidationErrors ()
validateNonEmpty input = failureIf (Text.null input) IsEmpty
