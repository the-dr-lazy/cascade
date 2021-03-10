{-|
Module      : Cascade.Data.Text.NonEmpty
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

{-# LANGUAGE DeriveDataTypeable #-}

module Cascade.Data.Text.NonEmpty (NonEmpty, pattern NonEmpty, un, mk, ValidationError(..)) where


import           Prelude                      hiding ( NonEmpty )
import qualified Prelude
import           Cascade.Data.Validation
import qualified Cascade.Data.Validation            as Validation
import           Data.Data                           ( Data )
import           Control.Lens.TH                     ( makeWrapped )
import           Data.Aeson                          ( FromJSON
                                                     , ToJSON
                                                     )
import qualified Data.Text                          as Text

newtype NonEmpty = Mk
  { un :: Text }
  deriving newtype (Show, Eq, FromJSON, ToJSON)

makeWrapped ''NonEmpty

pattern NonEmpty :: Text -> NonEmpty
pattern NonEmpty a <- Mk a
{-# COMPLETE NonEmpty #-}

mk :: Text -> Maybe NonEmpty
mk input = if Text.null input then Nothing else Just $ Mk input

data ValidationError = IsEmpty
  deriving stock (Generic, Data, Show)
  deriving ToJSON via (ApiErrorFormat ValidationError)

type ValidationErrors = Prelude.NonEmpty ValidationError

instance Validation.ToMessage ValidationError where
  toMessage IsEmpty = "empty text"

instance Validatable Text NonEmpty where
  type Errors Text NonEmpty = ValidationErrors

  parse = pure . maybeToSuccess (IsEmpty :| []) . mk
