{-|
Module      : Cascade.Api.Network.Server.Api.Projects.Tasks
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.Server.Api.Projects.Tasks (server) where

import qualified Cascade.Api.Data.Project           as Project
import qualified Cascade.Api.Data.Task              as Task
import qualified Cascade.Api.Effect.Database.Project
                                                    as Database.Project
import           Cascade.Api.Effect.Database.Project ( ProjectL )
import qualified Cascade.Api.Effect.Database.Task   as Database.Task
import           Cascade.Api.Effect.Database.Task    ( TaskL )
import qualified Cascade.Api.Effect.Time            as Time
import           Cascade.Api.Effect.Time             ( TimeL )
import           Cascade.Api.Network.Anatomy.Api.Projects.Tasks
import qualified Cascade.Api.Servant.Response       as Response
import qualified Cascade.Data.Validation            as Validation
import           Polysemy                            ( Members
                                                     , Sem
                                                     )
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic              ( AsServerT
                                                     , genericServerT
                                                     )
import           Validation                          ( validation )

handleCreate :: Members '[TaskL , ProjectL , TimeL] r => Project.Id -> Task.Creatable 'Validation.Raw -> Sem r (Union CreateResponse)
handleCreate projectId creatable = Task.parseRawCreatable creatable >>= validation (respond . Response.Unprocessable) go
 where
  go :: Members '[TaskL , ProjectL , TimeL] r => Task.Creatable 'Validation.Parsed -> Sem r (Union CreateResponse)
  go parsedCreatable = do
    projectExists <- Database.Project.doesExistsById projectId
    -- FIXME: boolean blindness
    if projectExists then Database.Task.create parsedCreatable projectId >>= respond . Response.created else respond Response.notFound

handleGetAllByProjectId :: Members '[TaskL , ProjectL] r => Project.Id -> Sem r (Union GetAllByProjectIdResponse)
handleGetAllByProjectId projectId = do
  projectExists <- Database.Project.doesExistsById projectId
  -- FIXME: boolean blindness
  if projectExists then Database.Task.findByProjectId projectId >>= respond . Response.ok else respond Response.notFound

server :: Members '[TaskL , ProjectL , TimeL] r => Project.Id -> ToServant Routes (AsServerT (Sem r))
server projectId = genericServerT Routes { getAllByProjectId = handleGetAllByProjectId projectId, create = handleCreate projectId }
