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

module Cascade.Core.Data.Model.User (User.Id, User(..), EmailAddress, Password, Username, unsafePhaseCoerce) where

import           Cascade.Core.Data.Model.Hashed      ( Hashed )
import qualified Cascade.Core.Data.Model.Phase      as Phase
import           Cascade.Core.Data.Model.User.EmailAddress
                                                     ( EmailAddress )
import           Cascade.Core.Data.Model.User.Password
                                                     ( Password )
import           Cascade.Core.Data.Model.User.Username
                                                     ( Username )
import qualified Cascade.Core.Internal.Data.Model.Task.Id
                                                    as Task
import qualified Cascade.Core.Internal.Data.Model.User.Id
                                                    as User
import           Unsafe.Coerce

data User phase = User
  { id                 :: User.Id phase
  , username           :: Username phase
  , emailAddress       :: EmailAddress phase
  , hashedPassword     :: Hashed Password
  , currentWorkingTask :: Maybe (Task.Id 'Phase.Persisted)
  }

type role User nominal

unsafePhaseCoerce :: User p -> User p'
unsafePhaseCoerce = unsafeCoerce
