{-|
Module      : Cascade.Core.Internal.Data.Contract.Database.SQL.Query.User
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

module Cascade.Core.Internal.Data.Contract.Database.SQL.Query.User (byId, byUsername) where

import qualified Cascade.Core.Data.Model.User       as User
import           Cascade.Core.Internal.Data.Contract.Database.SQL
                                                     ( Q )
import qualified Cascade.Core.Internal.Data.Contract.Database.SQL
                                                    as SQL
import qualified Cascade.Core.Internal.Data.Contract.Database.SQL.Query
                                                    as SQL.Query
import           Cascade.Core.Internal.Data.Contract.Database.UserTable
                                                     ( UserTable )
import qualified Cascade.Core.Internal.Data.Model.User.Id
                                                    as User.Id
import qualified Cascade.Core.Internal.Data.Model.User.Username
                                                    as User.Username
import qualified Database.Beam                      as Beam

byId :: _ => User.Id p -> Q backend s (UserTable (Beam.QExpr backend s))
byId (User.Id.un -> id) = SQL.Query.all #users |> SQL.filter (#id `SQL.eq` SQL.literal id)

byUsername :: _ => User.Username p -> Q backend s (UserTable (Beam.QExpr backend s))
byUsername (User.Username.un -> username) = SQL.Query.all #users |> SQL.filter (#username `SQL.eq` SQL.literal username)