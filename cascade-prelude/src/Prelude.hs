{-|
Module      : Prelude
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Prelude (module Data.Map.Strict, module Data.UUID, module Flow, module Relude) where

import           Data.Map.Strict                     ( Map )
import           Data.UUID                           ( UUID )
import           Flow                                ( (!>)
                                                     , (<!)
                                                     , (<|)
                                                     , (|>)
                                                     )
import           Relude                       hiding ( id )
