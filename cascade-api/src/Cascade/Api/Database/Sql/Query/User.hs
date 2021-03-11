{-|
Module      : Cascade.Api.Database.Sql.Query.User
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

module Cascade.Api.Database.Sql.Query.User
  ( byId
  , byUsername
  ) where

import qualified Cascade.Api.Data.User         as User
import           Cascade.Api.Data.WrappedC      ( WrappedC(..) )
import           Cascade.Api.Database.Sql       ( Q )
import qualified Cascade.Api.Database.Sql      as SQL
import qualified Cascade.Api.Database.Sql.Query
                                               as SQL.Query
import           Cascade.Api.Database.UserTable
import qualified Database.Beam                 as Beam

byId :: _ => User.Id -> Q backend s (UserTable (Beam.QExpr backend s))
byId id = SQL.Query.all #users |> SQL.filter (#id `SQL.eq` SQL.literal id)

byUsername :: _
           => User.Username
           -> Q backend s (UserTable (Beam.QExpr backend s))
byUsername username =
  SQL.Query.all #users |> SQL.filter (#username `SQL.eq` SQL.literal username)
