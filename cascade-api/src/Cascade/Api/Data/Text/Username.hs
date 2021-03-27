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

module Cascade.Api.Data.Text.Username (Username, Error(..), Errors, pattern Username, un, mk) where


import qualified Cascade.Data.Char                  as Char
import           Cascade.Data.Validation             ( Validation )
import qualified Cascade.Data.Validation            as Validation
import           Control.Lens.TH                     ( makeWrapped )
import           Control.Selective                   ( ifS )
import           Data.Aeson                          ( FromJSON
                                                     , ToJSON
                                                     )

import qualified Cascade.Api.Data.Aeson.FieldErrorFormat
                                                    as Aeson
import qualified Data.Text                          as Text

newtype Username = Mk
  { un :: Text }
  deriving newtype (Show, Eq, FromJSON, ToJSON)

makeWrapped ''Username

pattern Username :: Text -> Username
pattern Username a <- Mk a
{-# COMPLETE Username #-}

data Error
  = IsEmpty
  | IsShort
  | IsLong
  | IsInvalid
  deriving stock (Generic, Show)
  deriving ToJSON via Aeson.FieldErrorFormat Error
  deriving FromJSON via Aeson.FieldErrorFormat Error

type Errors = NonEmpty Error

mk :: Text -> Validation Errors Username
mk input = Mk input <$ validate input

type instance Validation.Errors Text Username = Errors

validate :: Text -> Validation Errors ()
validate input = ifS
  (pure $ Text.null input)
  (Validation.failure IsEmpty)
  (  Validation.failureIf (l > 20) IsLong
  *> Validation.failureIf (l < 8) IsShort
  *> Validation.failureUnless (Text.all Char.isAlphaNumUnderscore input) IsInvalid
  )
  where l = Text.length input
