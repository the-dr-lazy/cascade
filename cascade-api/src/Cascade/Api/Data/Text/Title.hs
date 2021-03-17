module Cascade.Api.Data.Text.Title (Title(..), mk) where

import qualified Cascade.Data.Text.NonEmpty         as Text.NonEmpty
import           Control.Lens.TH                     ( makeWrapped )
import           Data.Data                           ( Data )
import           Cascade.Data.Validation
import qualified Cascade.Data.Validation            as Validation
import           Data.Aeson                          ( FromJSON
                                                     , ToJSON
                                                     )

newtype Title = Mk
  { un :: Text }
  deriving stock Show
  deriving newtype (Eq, FromJSON, ToJSON)

makeWrapped ''Title

mk :: Text -> Maybe Title
mk t = case Text.NonEmpty.mk t of
  Just _  -> Just $ Mk t
  Nothing -> Nothing

data ValidationError = IsEmpty
  deriving stock (Generic, Data, Show)
  deriving ToJSON via (ApiErrorFormat ValidationError)

instance Validation.ToMessage ValidationError where
  toMessage IsEmpty = "empty title"

type ValidationErrors = NonEmpty ValidationError

instance Validatable Text Title where
  type Errors Text Title = ValidationErrors

  parse = pure . maybeToSuccess (IsEmpty :| []) . mk
