{-|
Module      : Cascade.Api.Data.Text.Username
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Data.Text.Username (Username, ValidationError(..), ValidationErrors, pattern Username, un, mk) where


import qualified Cascade.Data.Char                  as Char
import           Cascade.Data.Validation
import qualified Cascade.Data.Validation            as Validation
import           Control.Lens.TH                     ( makeWrapped )
import           Control.Selective                   ( ifS )
import           Data.Aeson                          ( FromJSON
                                                     , ToJSON
                                                     )
import           Data.Data                           ( Data )
import qualified Data.Text                          as Text
import qualified Polysemy

newtype Username = Mk
  { un :: Text }
  deriving newtype (Show, Eq, FromJSON, ToJSON)

makeWrapped ''Username

pattern Username :: Text -> Username
pattern Username a <- Mk a
{-# COMPLETE Username #-}

data ValidationError
  = IsEmpty
  | IsShort
  | IsLong
  | IsInvalid
  deriving stock (Generic, Data, Show)
  deriving ToJSON via (ApiErrorFormat ValidationError)

instance Validation.ToMessage ValidationError where
  toMessage = \case
    IsEmpty   -> "can't be empty"
    IsShort   -> "should have at least 8 characters"
    IsLong    -> "should not exceed 20 characters"
    IsInvalid -> "invalid characters"

type ValidationErrors = NonEmpty ValidationError

mk :: Text -> Validation ValidationErrors Username
mk = Polysemy.run . validate @Username

instance Validatable Username where
  type Raw Username = Text
  type Errors Username = ValidationErrors

  validate input = pure $ Mk input <$ ifS
    (pure $ Text.null input)
    (failure IsEmpty)
    (failureIf (l > 20) IsLong *> failureIf (l < 8) IsShort *> failureUnless (Text.all Char.isAlphaNumUnderscore input) IsInvalid)
    where l = Text.length input
