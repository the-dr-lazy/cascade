{-|
Module      : Test.Cascade.Api.StateMachine.Model.Lens
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Test.Cascade.Api.StateMachine.Model.Lens (indexTokenByUsername, indexPasswordByUsername) where

import           Cascade.Api.Network.TestClient      ( AuthToken )
import           Control.Lens
import           Hedgehog
import           Test.Cascade.Api.StateMachine.Model

indexTokenByUsername :: IndexedFold Username (Model v) (Var AuthToken v)
indexTokenByUsername = #authToken . #byUsername . ifolded

indexPasswordByUsername :: IndexedFold Username (Model v) Password
indexPasswordByUsername = #user . #byUsername . ifolded <. #password
