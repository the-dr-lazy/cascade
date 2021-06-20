{-|
Module      : Cascade.Api.Network.TestClient.Api.User.Projects
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.TestClient.Api.User.Projects
    ( CreateResponse
    , GetAllResponse
    , create
    , getAll
    ) where

import qualified Cascade.Api.Data.Project                      as Project
import qualified Cascade.Api.Network.Anatomy.Api.User.Projects as Api.User.Projects
import           Cascade.Api.Network.TestClient
    ( AuthToken, authenticated, interpret )
import qualified Cascade.Api.Network.TestClient.Api.User       as Client.Api.User
import           Control.Lens                                  ( (^.) )
import           Data.Generics.Labels                          ()
import           Prelude                                       hiding ( getAll )
import           Servant.API                                   ( Union )
import           Servant.Client.Free                           ( ResponseF )

type CreateResponse = (ResponseF (Union Api.User.Projects.CreateResponse))

create :: AuthToken -> Project.Creatable -> IO CreateResponse
create auth = interpret . flip go (authenticated auth) where go = Client.Api.User.projects ^. #create

type GetAllResponse = (ResponseF (Union Api.User.Projects.GetAllResponse))

getAll :: AuthToken -> IO GetAllResponse
getAll = interpret . go . authenticated where go = Client.Api.User.projects ^. #getAll
