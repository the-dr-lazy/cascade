{-|
Module      : Cascade.Api.Query.Project
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Query.Project
  ( queryAll
  , queryById
  ) where

import qualified Cascade.Api.Data.Project      as Project
import           Cascade.Api.Data.WrappedC
import           Cascade.Api.Database.ProjectTable
                                               as ProjectTable
import qualified Cascade.Api.Query             as Query
import           Cascade.Api.Query              ( Q )
import           Control.Lens            hiding ( (|>) )
import           Database.Beam           hiding ( Q )
import           Database.Beam.Backend

queryAll :: BeamSqlBackend backend
         => Q backend s (ProjectTable (QExpr backend s))
queryAll = Query.all #projects

queryById :: BeamSqlBackend backend
          => BeamSqlBackendCanSerialize backend (WrappedC Project.Id)
          => HasSqlEqualityCheck backend (WrappedC Project.Id)
          => Project.Id
          -> Q backend s (ProjectTable (QExpr backend s))
queryById id =
  queryAll |> filter_ \project -> project ^. #id ==. val_ (coerce id)
