module Cascade.Network.Anatomy
  ( Routes(..)
  ) where


import qualified Cascade.Network.Anatomy.Api   as Api
import           Data.Generics.Labels           ( )
import           Servant
import           Servant.API.Generic

data Routes route = Routes
  { api :: route :- "api" :> ToServantApi Api.Routes
  }
  deriving stock Generic
