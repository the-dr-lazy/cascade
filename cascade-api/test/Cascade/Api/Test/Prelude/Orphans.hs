{-|
Module      : Cascade.Api.Test.Prelude.Orphans
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cascade.Api.Test.Prelude.Orphans
  () where

import qualified Network.HTTP.Types.Status     as Http
                                                ( Status(..) )

deriving stock instance Generic Http.Status
