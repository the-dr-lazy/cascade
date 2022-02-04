{-|
Module      : Cascade.Logger.Message
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2022 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Logger.Message
    ( Minimal (..)
    , Scope (..)
    , Scoped (..)
    , prettyPrintScoped
    ) where

import qualified Cascade.Chronos           as Choronos
import           Cascade.Logger.Formatting ( color, square )
import           Cascade.Logger.Severity
import           Chronos                   ( DatetimeFormat (..), Time )
import qualified Data.Text.Lazy.Builder    as TB
import           System.Console.ANSI       ( Color (Cyan) )

data Minimal = Minimal { message  :: Text
                       , severity :: Severity
                       , time     :: Time
                       , cs       :: CallStack
                       }

prettyPrintMinimal :: Minimal -> Text
prettyPrintMinimal Minimal {..} = prettyPrintSeverity severity cs <> prettyPrintTime time <> message

prettyPrintTime :: Time -> Text
prettyPrintTime = square . toStrict . TB.toLazyText . Choronos.builderDbyHMSz format . Choronos.timeToDatetime
  where format = DatetimeFormat (Just ' ') (Just ' ') (Just ':')

data Scope = Cli | Api deriving stock (Eq)

prettyPrintScope :: Scope -> Text
prettyPrintScope scope = color Cyan . square <| text
 where
  text = case scope of
    Cli -> "CLI"
    Api -> "API"

data Scoped = Scoped Scope Minimal

prettyPrintScoped :: Scoped -> Text
prettyPrintScoped (Scoped scope minimal) = prettyPrintScope scope <> prettyPrintMinimal minimal
