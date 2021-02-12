module Cascade.Api.Data.Text.NonEmpty
  ( NonEmptyText
  , ValidationError(..)
  , ValidationErrors
  , pattern NonEmptyText
  , un
  , mk
  ) where


import qualified Cascade.Data.Char             as Char
import           Control.Lens.TH                ( makeWrapped )
import           Control.Selective              ( ifS )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import qualified Data.Text                     as Text
import           Validation

newtype NonEmptyText = Mk
  { un :: Text }
  deriving newtype (Show, Eq, FromJSON, ToJSON)

makeWrapped ''NonEmptyText

pattern NonEmptyText :: Text -> NonEmptyText
pattern NonEmptyText a <- Mk a
{-# COMPLETE NonEmptyText #-}

data ValidationError
  = IsEmpty
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type ValidationErrors = NonEmpty ValidationError

mk :: Text -> Validation ValidationErrors NonEmptyText
mk input = Mk input <$ validate input

validate :: Text -> Validation ValidationErrors ()
validate input = failureIf (Text.null input) IsEmpty
