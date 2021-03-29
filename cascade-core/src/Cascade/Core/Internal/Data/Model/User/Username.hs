{-|
Module      : Cascade.Core.Internal.Data.Text.Username
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Core.Internal.Data.Model.User.Username (Username, Error(..), Errors, pattern Username, un, mk, unsafePhaseCoerce) where

import           Cascade.Core.Data.Phase             ( Phase )
import qualified Cascade.Core.Data.Phase            as Phase
import qualified Cascade.Data.Char                  as Char
import           Control.Selective                   ( ifS )
import qualified Data.Text                          as Text
import           Validation                          ( Validation )
import qualified Validation

newtype Username (phase :: Phase) = Mk { un :: Text }
  deriving newtype (Show, Eq)

type role Username nominal

pattern Username :: Text -> Username phase
pattern Username a <- Mk a
{-# COMPLETE Username #-}

data Error
  = IsEmpty
  | IsShort
  | IsLong
  | IsInvalid
  deriving stock Show

type Errors = NonEmpty Error

mk :: Text -> Validation Errors (Username 'Phase.Unknown)
mk input = Mk input <$ validate input

validate :: Text -> Validation Errors ()
validate input = ifS
  (pure $ Text.null input)
  (Validation.failure IsEmpty)
  (  Validation.failureIf (l > 20) IsLong
  *> Validation.failureIf (l < 8) IsShort
  *> Validation.failureUnless (Text.all Char.isAlphaNumUnderscore input) IsInvalid
  )
  where l = Text.length input

unsafePhaseCoerce :: Username p -> Username p'
unsafePhaseCoerce = coerce
