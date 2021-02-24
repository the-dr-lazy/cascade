{-|
Module      : Cascade.Api.Database.Query.Project
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

module Cascade.Api.Database.Query.Project
  ( byId
  , byUser
  , byUserId
  ) where

import qualified Cascade.Api.Data.Project      as Project
import qualified Cascade.Api.Data.User         as User
import qualified Cascade.Api.Data.WrappedC     as WrappedC
import           Cascade.Api.Database.ProjectTable
                                               as ProjectTable
import qualified Cascade.Api.Database.Query    as Query
import           Cascade.Api.Database.Query     ( Q
                                                , filterProjectsByRelatedUsers
                                                )
import qualified Cascade.Api.Database.Query.User
                                               as Query.User
import           Cascade.Api.Database.UserTable ( UserTable )
import           Control.Lens                   ( (^.) )
import           Database.Beam           hiding ( Q )

byId :: _ => Project.Id -> Q backend s (ProjectTable (QExpr backend s))
byId id =
  Query.all #projects |> filter_ \user -> user ^. #id ==. WrappedC.val id

byUser :: _
       => Q backend s (UserTable (QExpr backend s))
       -> Q backend s (ProjectTable (QExpr backend s))
byUser q = Query.all #projects |> filterProjectsByRelatedUsers q

byUserId :: _ => User.Id -> Q backend s (ProjectTable (QExpr backend s))
byUserId = byUser . Query.User.byId
