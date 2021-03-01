{-|
Module      : Cascade.Api.Data.OffsetDatetime.Deadline
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Data.OffsetDatetime.Deadline (Deadline, un, mk) where

import           Cascade.Data.Chronos.Future
import           Chronos                             ( OffsetDatetime )

type Deadline = Future OffsetDatetime
