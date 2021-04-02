{-|
Module      : Cascade.CLI.Data.HttpPort
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.CLI.Data.HttpPort (HttpPort, Error(..), Errors, un, mk, def) where

import           Cascade.Data.Validation             ( Validation )
import qualified Cascade.Data.Validation            as Validation
import qualified Control.Exception                  as X
import qualified Network.Socket                     as Socket
import           System.IO.Error                     ( IOError )

newtype HttpPort = Mk
  { un :: Int }
  deriving stock (Show, Eq)

data Error = BusyHttpPortError
  deriving stock (Generic, Show)

type Errors = NonEmpty Error

type instance Validation.Errors (Last Int) HttpPort = Errors

mk :: Int -> IO (Validation Errors HttpPort)
mk httpPort = do
  x <- X.try @IOError (Socket.getAddrInfo Nothing (Just ("127.0.0.1" <> show httpPort)) (Just "http"))
  pure case x of
    Right _ -> Validation.Failure (BusyHttpPortError :| [])
    Left  _ -> Validation.Success <| Mk httpPort

def :: HttpPort
def = Mk 3141
