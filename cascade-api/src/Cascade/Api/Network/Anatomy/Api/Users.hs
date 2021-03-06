{-|
Module      : Cascade.Api.Network.Anatomy.Api.Users
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Network.Anatomy.Api.Users
    ( CreateResponse
    , Routes (..)
    ) where

import qualified Cascade.Api.Data.User               as User
import           Cascade.Api.Network.Anatomy.Prelude
import qualified Cascade.Api.Servant.Response        as Response
import qualified Cascade.Data.Validation             as Validation
import           Data.Generics.Labels                ()

type CreateResponse = '[Response.Created User.Readable , Response.Conflict , Response.Unprocessable (User.Creatable 'Validation.Error)]

data Routes route = Routes { create :: route :- ReqBody '[JSON] (User.Creatable Validation.Raw) :> Post '[JSON] CreateResponse
                           }
  deriving stock (Generic)
