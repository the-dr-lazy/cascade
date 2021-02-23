{-|
Module      : Cascade.Api.Network.Server.Api.Users
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.Server.Api.Users (server) where

import qualified Cascade.Api.Data.User              as User
import           Cascade.Api.Data.User               ( parseRawCreatableUser )
import qualified Cascade.Api.Effect.Database.User   as Database
                                                     ( UserL )
import qualified Cascade.Api.Effect.Database.User   as Database.User
import           Cascade.Api.Effect.Scrypt           ( ScryptL )
import           Cascade.Api.Network.Anatomy.Api.Users
import qualified Cascade.Api.Servant.Response       as Response
import           Cascade.Data.Validation             ( Phase(..)
                                                     , validation
                                                     )
import           Control.Lens                        ( (^.) )
import           Polysemy                            ( Members
                                                     , Sem
                                                     )
import           Servant                             ( Union
                                                     , respond
                                                     )
import           Servant.API.Generic                 ( ToServant )
import           Servant.Server.Generic

handleCreate :: Members '[Database.UserL , ScryptL] r => User.Creatable 'Raw -> Sem r (Union CreateResponse)
handleCreate = validation (respond . Response.Unprocessable) go . parseRawCreatableUser
 where
  go :: Members '[Database.UserL , ScryptL] r => User.Creatable 'Parsed -> Sem r (Union CreateResponse)
  go creatable = do
    hasConflict <- Database.User.doesExistsByUsernameOrEmailAddress (creatable ^. #username) (creatable ^. #emailAddress)
    if hasConflict then respond Response.Conflict else (respond . Response.Created) =<< Database.User.create creatable

server :: Members '[Database.UserL , ScryptL] r => ToServant Routes (AsServerT (Sem r))
server = genericServerT Routes { create = handleCreate }
