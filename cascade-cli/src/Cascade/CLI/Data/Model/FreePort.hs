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

import qualified Cascade.CLI.Data.Config.Default    as Config.Default
import qualified Control.Exception                  as X
import qualified Network.Socket                     as Socket
import           System.IO.Error                     ( IOError )

newtype FreePort = Mk
  { un :: Word16 }
  deriving stock (Show, Eq)

mk :: Last Word16 -> IO (Maybe FreePort)
mk (fromMaybe Config.Default.httpPort . getLast -> httpPort) =
  X.try @IOError (Socket.getAddrInfo Nothing (Just ("127.0.0.1" <> show httpPort)) (Just "http"))
    <&> either (const Nothing) (const . Just <| Mk httpPort)
