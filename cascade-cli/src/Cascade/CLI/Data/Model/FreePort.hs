{-|
Module      : Cascade.CLI.Data.Model.FreePort
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.CLI.Data.Model.FreePort (FreePort, un, mk) where

import           Cascade.CLI.Data.Errors             ( Error(..)
                                                     , Errors
                                                     )
import qualified Control.Exception                  as X
import qualified Network.Socket                     as Socket
import           System.IO.Error                     ( IOError )
import           Validation                          ( Validation )
import qualified Validation

newtype FreePort = Mk
  { un :: Int }
  deriving stock (Show, Eq)

mk :: Last Int -> IO (Validation Errors FreePort)
mk h = case getLast h of
  Nothing       -> pure <| Validation.Failure (EmptyField :| [])
  Just httpPort -> do
    x <- X.try @IOError (Socket.getAddrInfo Nothing (Just ("127.0.0.1" <> show httpPort)) (Just "http"))
    pure case x of
      Right _ -> Validation.Failure (BusyHttpPortError :| [])
      Left  _ -> Validation.Success <| Mk httpPort
