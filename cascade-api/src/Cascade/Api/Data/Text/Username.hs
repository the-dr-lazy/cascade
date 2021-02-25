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
import           Control.Lens.TH                     ( makeWrapped )
import           Control.Selective                   ( ifS )
import           Data.Aeson                          ( FromJSON
                                                     , ToJSON
                                                     )
import qualified Data.Text                          as Text
import           Validation

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
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type ValidationErrors = NonEmpty ValidationError

mk :: Text -> Validation ValidationErrors Username
mk input = Mk input <$ validate input

validate :: Text -> Validation ValidationErrors ()
validate input = ifS
  (pure $ Text.null input)
  (failure IsEmpty)
  (failureIf (l > 20) IsLong *> failureIf (l < 8) IsShort *> failureUnless (Text.all Char.isAlphaNumUnderscore input) IsInvalid)
  where l = Text.length input
