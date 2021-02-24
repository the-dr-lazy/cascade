{-|
Module      : Cascade.Api.Database.Query.User
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

module Cascade.Api.Database.Query.User
  ( byId
  , byUsername
  , existance
  ) where

import qualified Cascade.Api.Data.User         as User
import qualified Cascade.Api.Data.WrappedC     as WrappedC
import           Cascade.Api.Database.Query     ( Q )
import qualified Cascade.Api.Database.Query    as Query
import           Cascade.Api.Database.UserTable
import           Control.Lens                   ( (^.) )
import           Database.Beam           hiding ( Q )
import           Prelude                 hiding ( all )

byId :: _ => User.Id -> Q backend s (UserTable (QExpr backend s))
byId id = Query.all #users |> filter_ \user -> user ^. #id ==. WrappedC.val id

byUsername :: _ => User.Username -> Q backend s (UserTable (QExpr backend s))
byUsername username = Query.all #users
  |> filter_ \user -> user ^. #username ==. WrappedC.val username

existance :: _
          => (  Q backend s (UserTable (QExpr backend s))
             -> Q backend s (UserTable (QExpr backend s))
             )
          -> Q backend s (QExpr backend s Bool)
existance pipe = Query.all #users |> pipe |> exists_ |> pure
