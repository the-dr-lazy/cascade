{-|
Module      : Cascade.Core.Data.Model.User
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Core.Data.Model.User (User(..)) where

import           Cascade.Core.Data                   ( EmailAddress
                                                     , Hashed
                                                     , Id
                                                     , Password
                                                     , Username
                                                     )
import           Chronos                             ( Time )

data User phase = User
  { id             :: Id User phase
  , username       :: Username phase
  , emailAddress   :: EmailAddress phase
  , hashedPassword :: Hashed Password
  , createdAt      :: Time
  , updatedAt      :: Time
  }
