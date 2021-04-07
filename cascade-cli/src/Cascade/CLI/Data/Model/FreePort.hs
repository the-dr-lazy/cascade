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

import qualified Control.Exception                  as X
import qualified Network.Socket                     as Socket
import           System.IO.Error                     ( IOError )

newtype FreePort = Mk
  { un :: Word16 }
  deriving stock (Show, Eq)

mk :: Word16 -> IO (Maybe FreePort)
mk httpPort = do
  address : _ <- Socket.getAddrInfo (Just Socket.defaultHints { Socket.addrSocketType = Socket.Stream })
                                    (Just "127.0.0.1")
                                    (Just <| show httpPort)
  let action = X.bracket (Socket.openSocket address) Socket.close \socket -> Socket.connect socket (Socket.addrAddress address)
  X.try @IOError action |> fmap \case
    Left  _ -> Just <| Mk httpPort
    Right _ -> Nothing
