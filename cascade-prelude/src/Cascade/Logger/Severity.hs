{-|
Module      : Cascade.Logger.Severity
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Logger.Severity (Severity(..), prettyPrintSeverity) where

import           Cascade.Logger.Formatting
import qualified Data.Text                          as Text
import           GHC.Stack                           ( SrcLoc(..) )
import           System.Console.ANSI                 ( Color(..) )

data Severity
  = Debug
  | Info
  | Warning
  | Error
  | Panic
  deriving stock Eq

prettyPrintSeverity :: Severity -> CallStack -> Text
prettyPrintSeverity Debug   _  = color Green . square <| "Debug"
prettyPrintSeverity Info    _  = color Blue . square <| "Info"
prettyPrintSeverity Warning _  = color Yellow . square <| "Warning"
prettyPrintSeverity Error   _  = color Red . square <| "Error"
prettyPrintSeverity Panic   cs = color Red . square <| "Panic " <> prettyPrintStackTrace cs

prettyPrintStackTrace :: CallStack -> Text
prettyPrintStackTrace cs = case getCallStack cs of
  [] -> "<unknown location>"
  [(callerName, location)] -> prettyPrintStackTraceLocation callerName location
  (_, location) : (callerName, _) : _ -> prettyPrintStackTraceLocation callerName location

prettyPrintStackTraceLocation :: String -> SrcLoc -> Text
prettyPrintStackTraceLocation callerName SrcLoc {..} =
  Text.pack srcLocModule <> "." <> Text.pack callerName <> "#" <> Text.pack (show srcLocStartLine)
