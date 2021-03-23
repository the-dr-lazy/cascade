{-|
Module      : Cascade.Core.Data
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Core.Data (Id, Phase, Username, EmailAddress, Hashed, Password) where

import           Cascade.Core.Data.ByteString.Password
                                                     ( Password )
import           Cascade.Core.Data.Hashed            ( Hashed )
import           Cascade.Core.Data.Id                ( Id )
import           Cascade.Core.Data.Phase             ( Phase )
import           Cascade.Core.Data.Text.EmailAddress ( EmailAddress )
import           Cascade.Core.Data.Text.Username     ( Username )
