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

import qualified Cascade.Chronos                    as Choronos
import           Cascade.Log.Formatting
import           Cascade.Log.Severity
import           Chronos                             ( DatetimeFormat(..)
                                                     , Time(..)
                                                     )
import qualified Chronos
import           Colog.Actions                       ( logTextStderr
                                                     , logTextStdout
                                                     )
import           Colog.Core                          ( LogAction )
import           Colog.Core.Action                   ( cfilter )
import           Colog.Monad                         ( WithLog
                                                     , logMsg
                                                     )
import qualified Data.Text.Lazy.Builder             as TB
import           System.Console.ANSI                 ( Color(..) )

log :: WithLog env Message m => MonadIO m => Scope -> Severity -> Text -> m ()
log scope severity message = do
  time <- liftIO Chronos.now
  withFrozenCallStack (logMsg Message { location = callStack, .. })

logMessageStdout :: MonadIO m => LogAction m Message
logMessageStdout = prettyPrintMessage >$< logTextStdout

logMessageStderr :: MonadIO m => LogAction m Message
logMessageStderr = prettyPrintMessage >$< logTextStderr

logMessageStdoutAndStderr :: MonadIO m => LogAction m Message
logMessageStdoutAndStderr = logOut <> logErr
 where
  logErr = cfilter (\Message {..} -> severity == Error || severity == Panic) logMessageStderr
  logOut = cfilter (\Message {..} -> severity /= Error && severity /= Panic) logMessageStdout

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

prettyPrintMessage :: Message -> Text
prettyPrintMessage Message {..} = prettyPrintScope scope <> prettyPrintSeverity severity location <> prettyPrintTime time <> message

prettyPrintTime :: Time -> Text
prettyPrintTime = square . toStrict . TB.toLazyText . Choronos.builderDbyHMSz format . Chronos.timeToDatetime
  where format = DatetimeFormat (Just ' ') (Just ' ') (Just ':')
