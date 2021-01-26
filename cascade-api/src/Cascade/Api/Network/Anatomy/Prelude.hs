module Cascade.Api.Network.Anatomy.Prelude
  ( module Servant.API
  , module Servant.API.Generic
  , Get
  , Post
  , Patch
  , Delete
  ) where

import           Servant.API             hiding ( Delete
                                                , Get
                                                , Patch
                                                , Post
                                                )
import           Servant.API.Generic

type Get = UVerb 'GET

type Post = UVerb 'POST

type Patch = UVerb 'PATCH

type Delete = UVerb 'DELETE
