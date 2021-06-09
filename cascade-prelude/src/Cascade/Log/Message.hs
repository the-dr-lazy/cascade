{-|
Module      : Cascade.Log.Message
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Log.Message (log, Message, Scope(..), prettyPrintMessage, logMessageStdoutAndStderr, logMessageStderr, logMessageStdout) where

import           Cascade.Log.Severity                ( Severity(..) )
import           Chronos                             ( Time )
import qualified Chronos
import qualified Chronos.Locale.English             as Chronos
import           Colog.Actions                       ( logTextStderr
                                                     , logTextStdout
                                                     )
import           Colog.Core                          ( LogAction )
import           Colog.Core.Action                   ( cfilter )
import           Colog.Monad                         ( WithLog
                                                     , logMsg
                                                     )
import qualified Data.Text                          as Text
import qualified Data.Text.Lazy.Builder             as TB
import qualified Data.Text.Lazy.Builder.Int         as TB
import qualified Data.Vector                        as Vector
import           GHC.Stack                           ( SrcLoc(..) )
import           System.Console.ANSI                 ( Color(..)
                                                     , ColorIntensity(Vivid)
                                                     , ConsoleLayer(Foreground)
                                                     , SGR(..)
                                                     , setSGRCode
                                                     )

data Scope = Cli | Api
  deriving stock Eq

prettyPrintScope :: Scope -> Text
prettyPrintScope scope = color Cyan . square <| text
 where
  text = case scope of
    Cli -> "CLI"
    Api -> "API"

data Message = Message
  { message  :: Text
  , severity :: Severity
  , scope    :: Scope
  , time     :: Time
  , location :: CallStack
  }

log :: WithLog env Message m => MonadIO m => Scope -> Severity -> Text -> m ()
log scope severity message = do
  time <- liftIO Chronos.now
  withFrozenCallStack (logMsg Message { location = callStack, .. })

prettyPrintMessage :: Message -> Text
prettyPrintMessage Message {..} = prettyPrintScope scope <> showSeverity severity location <> prettyPrintTime time <> message

logMessageStdout :: MonadIO m => LogAction m Message
logMessageStdout = prettyPrintMessage >$< logTextStdout

logMessageStderr :: MonadIO m => LogAction m Message
logMessageStderr = prettyPrintMessage >$< logTextStderr

logMessageStdoutAndStderr :: MonadIO m => LogAction m Message
logMessageStdoutAndStderr = logOut <> logErr
 where
  logErr = cfilter (\Message {..} -> severity == Error || severity == Panic) logMessageStderr
  logOut = cfilter (\Message {..} -> severity /= Error && severity /= Panic) logMessageStdout

prettyPrintTime :: Time -> Text
prettyPrintTime t = square . toStrict . TB.toLazyText <| builderDmyHMSz (Chronos.timeToDatetime t)

builderDmyHMSz :: Chronos.Datetime -> TB.Builder
builderDmyHMSz (Chronos.Datetime date time) =
  builderDmy date <> spaceSep <> Chronos.builder_HMS (Chronos.SubsecondPrecisionFixed 3) (Just ':') time <> spaceSep <> Chronos.builderOffset
    Chronos.OffsetFormatColonOn
    (Chronos.Offset 0)
 where
  spaceSep :: TB.Builder
  spaceSep = TB.singleton ' '

  builderDmy :: Chronos.Date -> TB.Builder
  builderDmy (Chronos.Date (Chronos.Year y) m d) =
    zeroPadDayOfMonth d <> spaceSep <> TB.fromText (Chronos.caseMonth Chronos.abbreviated m) <> spaceSep <> TB.decimal y

  zeroPadDayOfMonth :: Chronos.DayOfMonth -> TB.Builder
  zeroPadDayOfMonth (Chronos.DayOfMonth d) = if d < 100 then Vector.unsafeIndex twoDigitTextBuilder d else TB.decimal d

  twoDigitTextBuilder :: Vector.Vector TB.Builder
  twoDigitTextBuilder = Vector.fromList . map (TB.fromText . Text.pack) <| twoDigitStrings
  {-# NOINLINE twoDigitTextBuilder #-}

  twoDigitStrings :: [String]
  twoDigitStrings = (\a b -> mappend (show @_ @Int a) (show @_ @Int b)) <$> [0 .. 9] <*> [0 .. 9]

prettyPrintStackTrace :: CallStack -> Text
prettyPrintStackTrace cs = case getCallStack cs of
  [] -> "<unknown location>"
  [(callerName, location)] -> prettyPrintStackTraceLocation callerName location
  (_, location) : (callerName, _) : _ -> prettyPrintStackTraceLocation callerName location

prettyPrintStackTraceLocation :: String -> SrcLoc -> Text
prettyPrintStackTraceLocation callerName SrcLoc {..} = Text.pack srcLocModule <> "." <> Text.pack callerName <> "#" <> Text.pack (show srcLocStartLine)

showSeverity :: Severity -> CallStack -> Text
showSeverity Debug   _  = color Green . square <| "Debug"
showSeverity Info    _  = color Blue . square <| "Info"
showSeverity Warning _  = color Yellow . square <| "Warning"
showSeverity Error   _  = color Red . square <| "Error"
showSeverity Panic   cs = color Red . square <| "Panic " <> prettyPrintStackTrace cs

square :: Text -> Text
square t = "[" <> t <> "] "

color :: Color -> Text -> Text
color c txt = Text.pack (setSGRCode [SetColor Foreground Vivid c]) <> txt <> Text.pack (setSGRCode [Reset])
