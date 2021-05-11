module Cascade.Colog.Message (Message(..), fmtMessage, log) where

import           Colog.Core                          ( Severity(..) )
import           Colog.Monad                         ( WithLog
                                                     , logMsg
                                                     )
import           Chronos                             ( Time )
import qualified Chronos
import qualified Chronos.Locale.English             as Chronos
import           Colog.Message                       ( showSeverity
                                                     , showSourceLoc
                                                     )
import           Control.Concurrent                  ( ThreadId
                                                     , myThreadId
                                                     )
import qualified Data.Text                          as Text
import qualified Data.Text.Lazy.Builder             as TB
import qualified Data.Text.Lazy.Builder.Int         as TB
import qualified Data.Vector                        as Vector

data Message = Message
  { text     :: Text
  , severity :: Severity
  , location :: CallStack
  , threadId :: ThreadId
  , time     :: Time
  }

log :: WithLog env Message m => MonadIO m => Severity -> Text -> m ()
log severity text = do
  time     <- liftIO Chronos.now
  threadId <- liftIO myThreadId
  withFrozenCallStack (logMsg Message { location = callStack, .. })

square :: Text -> Text
square t = "[" <> t <> "] "

showThreadId :: ThreadId -> Text
showThreadId = square . show

showTime :: Time -> Text
showTime t = square $ toStrict $ TB.toLazyText $ builderDmyHMSz (Chronos.timeToDatetime t)

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
  twoDigitTextBuilder = Vector.fromList $ map (TB.fromText . Text.pack) twoDigitStrings
  {-# NOINLINE twoDigitTextBuilder #-}

  twoDigitStrings :: [String]
  twoDigitStrings = (\a b -> mappend (show @_ @Int a) (show @_ @Int b)) <$> [0 .. 9] <*> [0 .. 9]

fmtMessage :: Message -> Text
fmtMessage Message {..} = showSeverity severity <> showTime time <> showSourceLoc location <> showThreadId threadId <> text
