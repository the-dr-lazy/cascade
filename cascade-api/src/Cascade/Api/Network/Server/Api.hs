{-|
Module      : Cascade.Api.Network.Server.Api
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.Server.Api (Effects, server) where

import qualified Cascade.Api.Effect.Database.Project
                                                    as Database
                                                     ( ProjectL )
import qualified Cascade.Api.Effect.Database.Task   as Database
                                                     ( TaskL )
import qualified Cascade.Api.Effect.Database.User   as Database
                                                     ( UserL )
import           Cascade.Api.Effect.Scrypt           ( ScryptL )
import           Cascade.Api.Effect.Time             ( TimeL )
import           Cascade.Api.Network.Anatomy.Api
import qualified Cascade.Api.Network.Server.Api.Authentication
                                                    as Authentication
import qualified Cascade.Api.Network.Server.Api.Projects
                                                    as Projects
import qualified Cascade.Api.Network.Server.Api.Tasks
                                                    as Tasks
import qualified Cascade.Api.Network.Server.Api.User
                                                    as User
import qualified Cascade.Api.Network.Server.Api.Users
                                                    as Users
import           Polysemy                            ( Members
                                                     , Sem
                                                     )
import           Polysemy.Error                      ( Error )
import           Servant.API.Generic
import           Servant.Server                      ( ServerError )
import           Servant.Server.Generic              ( AsServerT
                                                     , genericServerT
                                                     )

type Effects = '[Database.ProjectL , Database.UserL , Database.TaskL , TimeL , ScryptL , Error ServerError]

server :: Members Effects r => ToServant Routes (AsServerT (Sem r))
server = genericServerT Routes { authentication = Authentication.server
                               , projects       = Projects.server
                               , tasks          = Tasks.server
                               , user           = User.server
                               , users          = Users.server
                               }
