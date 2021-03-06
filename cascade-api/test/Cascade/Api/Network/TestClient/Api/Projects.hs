{-|
Module      : Cascade.Api.Network.TestClient.Api.Projects
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.TestClient.Api.Projects
    ( DeleteByIdResponse
    , GetByIdResponse
    , UpdateByIdResponse
    , deleteById
    , getById
    , tasks
    , testReadableVsCreatable
    , updateById
    , updateCreatable
    ) where

import qualified Cascade.Api.Data.Project                       as Project
import qualified Cascade.Api.Network.Anatomy.Api.Projects       as Api.Projects
import qualified Cascade.Api.Network.Anatomy.Api.Projects.Tasks as Api.Projects.Tasks
import           Cascade.Api.Network.TestClient
    ( AuthToken, authenticated, interpret )
import qualified Cascade.Api.Network.TestClient.Api             as Client.Api
import           Control.Lens                                   ( (^.) )
import           Control.Monad.Free                             ( Free )
import           Data.Generics.Labels                           ()
import           Hedgehog                                       ( Test, (===) )
import           Servant.API                                    ( Union )
import           Servant.API.Generic                            ( fromServant )
import           Servant.Client.Free                            ( ClientF, ResponseF )
import           Servant.Client.Generic                         ( AsClientT )

type GetByIdResponse = (ResponseF (Union Api.Projects.GetByIdResponse))

getById :: AuthToken -> Project.Id -> IO GetByIdResponse
getById auth = interpret . flip go (authenticated auth) where go = Client.Api.projects ^. #getById

type UpdateByIdResponse = (ResponseF (Union Api.Projects.UpdateByIdResponse))

updateById :: AuthToken -> Project.Id -> Project.Updatable -> IO UpdateByIdResponse
updateById auth id updatable = interpret $ go id updatable (authenticated auth) where go = Client.Api.projects ^. #updateById

type DeleteByIdResponse = (ResponseF (Union Api.Projects.DeleteByIdResponse))

deleteById :: AuthToken -> Project.Id -> IO DeleteByIdResponse
deleteById auth = interpret . flip go (authenticated auth) where go = Client.Api.projects ^. #deleteById

tasks :: Project.Id -> Api.Projects.Tasks.Routes (AsClientT (Free ClientF))
tasks projectId = fromServant (Client.Api.projects ^. #tasks $ projectId)

testReadableVsCreatable :: Project.Readable -> Project.Creatable -> Test ()
testReadableVsCreatable readable creatable = mkCreatableFromReadable readable === creatable

mkCreatableFromReadable :: Project.Readable -> Project.Creatable
mkCreatableFromReadable Project.Readable {..} = Project.Creatable { .. }

updateCreatable :: Project.Updatable -> Project.Creatable -> Project.Creatable
updateCreatable updatable Project.Creatable {..} = Project.Creatable { name = fromMaybe name $ updatable ^. #name }
