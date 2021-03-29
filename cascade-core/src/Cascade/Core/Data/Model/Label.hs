{-|
Module      : Cascade.Core.Data.Model.Label
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Core.Data.Model.Label (Label(..)) where

import           Cascade.Core.Data.Model.Id          ( Id )
import qualified Cascade.Data.Text                  as Text

data Label phase = Label
  { id   :: Id Label phase
  , name :: Text.Finite 1 233
  }
