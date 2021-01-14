module Cascade.Network.Anatomy.Api
  ( Routes(..)
  ) where

import qualified Cascade.Network.Anatomy.Api.Projects
                                               as Api.Projects
import           Data.Generics.Labels           ( )
import           Servant
import           Servant.API.Generic

data Routes route = Routes
  { projects :: route :- "projects" :> ToServantApi Api.Projects.Routes
  }
  deriving stock Generic
