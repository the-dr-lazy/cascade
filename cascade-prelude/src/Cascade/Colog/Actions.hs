module Cascade.Colog.Actions (logMessageStdout, logMessageStderr, logMessageStdoutAndStderr) where

import           Cascade.Colog.Message               ( Message(..)
                                                     , fmtMessage
                                                     )
import           Colog.Actions                       ( logTextStderr
                                                     , logTextStdout
                                                     )
import           Colog.Core                          ( LogAction
                                                     , Severity(..)
                                                     )
import           Colog.Core.Action                   ( cfilter )

logMessageStdout :: MonadIO m => LogAction m Message
logMessageStdout = fmtMessage >$< logTextStdout

logMessageStderr :: MonadIO m => LogAction m Message
logMessageStderr = fmtMessage >$< logTextStderr

logMessageStdoutAndStderr :: MonadIO m => LogAction m Message
logMessageStdoutAndStderr = log <> logError
 where
  logError = cfilter (\Message {..} -> severity == Error) logMessageStderr
  log      = cfilter (\Message {..} -> severity /= Error) logMessageStdout
