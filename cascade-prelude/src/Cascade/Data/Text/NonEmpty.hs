module Cascade.Data.Text.NonEmpty
  ( NonEmpty
  , ValidationError(..)
  , ValidationErrors
  , pattern NonEmpty
  , un
  , mk
  )
where


import           Prelude                 hiding ( NonEmpty )
import qualified Prelude                        ( NonEmpty )
import           Control.Lens.TH                ( makeWrapped )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import qualified Data.Text                     as Text
import           Validation

newtype NonEmpty = Mk
  { un :: Text }
  deriving newtype (Show, Eq, FromJSON, ToJSON)

makeWrapped ''NonEmpty

pattern NonEmpty :: Text -> NonEmpty
pattern NonEmpty a <- Mk a
{-# COMPLETE NonEmpty #-}

data ValidationError
  = IsEmpty
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type ValidationErrors = Prelude.NonEmpty ValidationError

mk :: Text -> Validation ValidationErrors NonEmpty
mk input = Mk input <$ validate input

validate :: Text -> Validation ValidationErrors ()
validate input = failureIf (Text.null input) IsEmpty
