{-|
Module      : Cascade.Core.Internal.Data.Model.User.Username
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Core.Internal.Data.Model.User.Username (Username, Error(..), Errors, pattern Username, un, mk, unsafePhaseCoerce) where

import           Cascade.Core.Data.Model.Phase       ( Phase )
import qualified Cascade.Core.Data.Model.Phase      as Phase
import qualified Cascade.Data.Char                  as Char
import qualified Cascade.Data.List                  as List
import qualified Cascade.Data.Text.Finite           as Text.Finite
import qualified Cascade.Data.Validation            as Validation
import qualified Data.Text                          as Text
import           Validation                          ( Validation )

newtype Username (phase :: Phase) = Mk { un :: Text }
  deriving newtype (Show, Eq)

type role Username nominal

pattern Username :: Text -> Username phase
pattern Username a <- Mk a
{-# COMPLETE Username #-}

data Error
  = InvalidLengthError Text.Finite.Error
  | InvalidCharactersError

type Errors = List.NonEmpty Error

mk :: Text -> Validation Errors (Username 'Phase.Unknown)
mk input = Mk input <$ validate input

validate :: Text -> Validation Errors ()
validate input =
  validateLength input `Validation.whenSuccess_` const (Validation.failureIf (Text.all Char.isAlphaNumUnderscore input) InvalidCharactersError)

validateLength :: Text -> Validation (NonEmpty Error) ()
validateLength = Validation.fromEither . first InvalidLengthError . Text.Finite.validate 8 21

unsafePhaseCoerce :: Username p -> Username p'
unsafePhaseCoerce = coerce
