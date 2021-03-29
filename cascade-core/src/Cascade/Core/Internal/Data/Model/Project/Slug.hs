{-|
Module      : Cascade.Core.Internal.Data.Text.Slug
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Core.Internal.Data.Model.Project.Slug (Slug, pattern Slug, mk, un, unsafePhaseCoerce) where

import           Cascade.Core.Data.Phase             ( Phase )
import qualified Cascade.Core.Data.Phase            as Phase
import qualified Cascade.Data.Char                  as Char
import qualified Data.Text                          as Text
import qualified Validation
import           Validation                          ( Validation )

-- brittany-disable-next-binding
newtype Slug (phase :: Phase) = Mk { un :: Text }

pattern Slug :: Text -> Slug phase
pattern Slug a <- Mk a

data Error
  = InvalidLengthError Word
  | InvalidCharactersError
  | DuplicateDashesError
  | LeadingDashesError
  | TrailingDashesError
  deriving stock Show

type Errors = NonEmpty Error

mk :: Text -> Validation Errors (Slug 'Phase.Unknown)
mk (Text.toLower . Text.strip -> input) = Mk input <$ validate input

validate :: Text -> Validation Errors ()
validate input = Validation.failureIf (Text.null input) (InvalidLengthError 0) `Validation.whenFailure_` const
  (  Validation.failureIf (l > 233) (InvalidLengthError l)
  *> Validation.failureIf ("-" `Text.isPrefixOf` input) LeadingDashesError
  *> Validation.failureIf ("-" `Text.isPrefixOf` input) TrailingDashesError
  *> Validation.failureIf (not . Text.all Char.isAlphaNumDash <| input) InvalidCharactersError
  )
  where l = fromIntegral . Text.length <| input

unsafePhaseCoerce :: Slug p -> Slug p'
unsafePhaseCoerce = coerce
