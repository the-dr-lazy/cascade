module Cascade.Api.Network.Server.Api.Tasks
  ( server
  )
where

import qualified Cascade.Api.Data.Task         as Task
import qualified Cascade.Api.Effect.Database.Task
                                               as Database.Task
import           Cascade.Api.Effect.Database.Task
                                                ( TaskL )
import           Cascade.Api.Effect.Time        ( TimeL
                                                , now
                                                )
import           Cascade.Api.Network.Anatomy.Api.Tasks
import qualified Cascade.Api.Servant.Response  as Response
import           Polysemy                       ( Member
                                                , Members
                                                , Sem
                                                )
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic         ( AsServerT
                                                , genericServerT
                                                )
import           Validation                     ( validation )

handleGetById :: Member TaskL r => Task.Id -> Sem r (Union GetByIdResponse)
handleGetById id = Database.Task.findById id
  >>= maybe (respond Response.notFound) (respond . Response.ok)

handleUpdateById
  :: Members '[TaskL, TimeL] r
  => Task.Id
  -> Task.RawUpdatable
  -> Sem r (Union UpdateByIdResponse)
handleUpdateById id updatable =
  now
    >>= validation (respond . Response.Unprocessable) go
    .   Task.parseRawUpdatableTask updatable
 where
  go
    :: Members '[TaskL, TimeL] r
    => Task.ParsedUpdatable
    -> Sem r (Union UpdateByIdResponse)
  go parsedUpdatable = Database.Task.updateById id parsedUpdatable
    >>= maybe (respond Response.notFound) (respond . Response.ok)

handleDeleteById
  :: Member TaskL r => Task.Id -> Sem r (Union DeleteByIdResponse)
handleDeleteById id = Database.Task.deleteById id
  >>= maybe (respond Response.notFound) (respond . Response.ok)

server :: Members '[TaskL, TimeL] r => ToServant Routes (AsServerT (Sem r))
server = genericServerT Routes { getById    = handleGetById
                               , updateById = handleUpdateById
                               , deleteById = handleDeleteById
                               }
