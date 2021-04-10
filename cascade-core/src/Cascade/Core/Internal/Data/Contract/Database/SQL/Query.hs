{-|
Module      : Cascade.Core.Internal.Data.Contract.Database.SQL.Query
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Cascade.Core.Internal.Data.Contract.Database.SQL.Query (all, existance) where

import           Cascade.Core.Internal.Data.Contract.Database
import           Cascade.Core.Internal.Data.Contract.Database.SQL
import           Control.Lens                        ( (^.) )
import qualified Database.Beam                      as Beam
import           Prelude                      hiding ( all )

all :: _ => DatabaseEntityGetting backend table -> Q backend s (table (Beam.QExpr backend s))
all = Beam.all_ . (database ^.)

existance :: _
          => DatabaseEntityGetting backend table
          -> (Q backend s (table (Beam.QExpr backend s)) -> Q backend s (table (Beam.QExpr backend s)))
          -> Q backend s (Beam.QExpr backend s Bool)
existance optic pipe = pure (all optic |> pipe |> Beam.exists_)
