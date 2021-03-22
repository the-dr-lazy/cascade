module Cascade.Api.Data.Text.Title (Title(..), mk, parse) where

import qualified Cascade.Api.Data.Aeson.FieldErrorFormat
                                                    as Aeson
import qualified Cascade.Data.Text.NonEmpty         as Text.NonEmpty
import           Cascade.Data.Validation             ( Validation )
import qualified Cascade.Data.Validation            as Validation
import           Control.Lens.TH                     ( makeWrapped )
import           Data.Aeson                          ( FromJSON
                                                     , ToJSON
                                                     )
import           Data.Data                           ( Data )

newtype Title = Mk
  { un :: Text }
  deriving stock Show
  deriving newtype (Eq, FromJSON, ToJSON)

-- FIXME: constructor leak
makeWrapped ''Title

mk :: Text -> Maybe Title
mk t = case Text.NonEmpty.mk t of
  Just _  -> Just $ Mk t
  Nothing -> Nothing

data Error = IsEmpty
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via (Aeson.FieldErrorFormat Error)

type Errors = NonEmpty Error

type instance Validation.Errors Text Title = Errors

parse :: Text -> Validation Errors Title
parse = Validation.maybeToSuccess (IsEmpty :| []) . mk
