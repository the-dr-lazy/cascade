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

module Cascade.Api.Network.Server.Api.Users
  ( server
  ) where

import qualified Cascade.Api.Data.User         as User
import           Cascade.Api.Data.User          ( parseRawCreatableUser )
import qualified Cascade.Api.Effect.Depository as Depository
import qualified Cascade.Api.Effect.Depository.User
                                               as Depository.User
import           Cascade.Api.Effect.Scrypt      ( ScryptL )
import           Cascade.Api.Network.Anatomy.Api.Users
import qualified Cascade.Api.Servant.Response  as Response
import           Control.Lens                   ( (^.) )
import           Polysemy                       ( Members
                                                , Sem
                                                )
import           Servant                        ( Union
                                                , respond
                                                )
import           Servant.API.Generic            ( ToServant )
import           Servant.Server.Generic
import           Validation                     ( validation )

handleCreate :: Members '[Depository.UserL , ScryptL] r
             => User.RawCreatable
             -> Sem r (Union CreateResponse)
handleCreate =
  validation (respond . Response.Unprocessable) go . parseRawCreatableUser
 where
  go :: Members '[Depository.UserL , ScryptL] r
     => User.ParsedCreatable
     -> Sem r (Union CreateResponse)
  go creatable = do
    hasConflict <- Depository.User.doesExistsByUsernameOrEmailAddress
      (creatable ^. #username)
      (creatable ^. #emailAddress)
    if hasConflict
      then respond Response.Conflict
      else (respond . Response.Created) =<< Depository.User.create creatable

server :: Members '[Depository.UserL , ScryptL] r
       => ToServant Routes (AsServerT (Sem r))
server = genericServerT Routes { create = handleCreate }
