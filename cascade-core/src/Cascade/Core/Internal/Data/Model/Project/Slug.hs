{-|
Module      : Cascade.Core.Internal.Data.Model.Project.Slug
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Core.Internal.Data.Model.Project.Slug (Slug, pattern Slug, mk, un, unsafePhaseCoerce) where

import           Cascade.Core.Data.Model.Phase       ( Phase )
import qualified Cascade.Core.Data.Model.Phase      as Phase
import qualified Cascade.Data.Char                  as Char
import qualified Cascade.Data.List                  as List
import qualified Cascade.Data.Text.Finite           as Text.Finite
import qualified Cascade.Data.Validation            as Validation
import qualified Data.Text                          as Text
import           Validation                          ( Validation )

-- brittany-disable-next-binding
newtype Slug (phase :: Phase) = Mk { un :: Text }

pattern Slug :: Text -> Slug phase
pattern Slug a <- Mk a

data Error
  = InvalidLengthError Text.Finite.Error
  | InvalidCharactersError
  | DuplicateDashesError
  | LeadingDashesError
  | TrailingDashesError

type Errors = List.NonEmpty Error

mk :: Text -> Validation Errors (Slug 'Phase.Unknown)
mk (Text.toLower . Text.strip -> input) = Mk input <$ validate input

validate :: Text -> Validation Errors ()
validate input = validateLength input `Validation.whenSuccess_` const
  (  Validation.failureIf ("-" `Text.isPrefixOf` input) LeadingDashesError
  *> Validation.failureIf ("-" `Text.isPrefixOf` input) TrailingDashesError
  *> Validation.failureIf ("--" `Text.isInfixOf` input) DuplicateDashesError
  *> Validation.failureIf (not . Text.all Char.isAlphaNumDash <| input) InvalidCharactersError
  )

validateLength :: Text -> Validation Errors ()
validateLength = Validation.fromEither . first InvalidLengthError . Text.Finite.validate 1 233

unsafePhaseCoerce :: Slug p -> Slug p'
unsafePhaseCoerce = coerce
