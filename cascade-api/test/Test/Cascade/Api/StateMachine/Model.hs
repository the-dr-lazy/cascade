{-|
Module      : Test.Cascade.Api.StateMachine.Model
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Test.Cascade.Api.StateMachine.Model
  ( Model
  , Username
  , EmailAddress
  , Password
  , initialModel
  , mapUsernameTokenProjectIdAList
  , getUsernameTokenProjectIdAList
  ) where

import qualified Cascade.Api.Data.Project      as Project
import qualified Cascade.Api.Data.User         as User
import           Cascade.Api.Network.TestClient ( AuthToken )
import           Control.Lens                   ( (^..)
                                                , at
                                                , foldMapOf
                                                , folded
                                                , to
                                                )
import           Data.Generics.Labels           ( )
import qualified Data.Map.Strict               as Map
import           Hedgehog.Internal.State        ( Var )

-- brittany-disable-next-binding
data Model (v :: Type -> Type) = Model
  { project   :: ProjectModel v
  , user      :: UserModel
  , authToken :: AuthTokenModel v
  }
  deriving stock Generic

initialModel :: Model v
initialModel = Model
  { project   = ProjectModel { byUsername = Map.empty, notExistingIds = mempty }
  , user      = UserModel { byUsername = Map.empty, byEmailAddress = Map.empty }
  , authToken = AuthTokenModel { byUsername = Map.empty }
  }

type Username = Text
type EmailAddress = Text
type Password = Text

-- brittany-disable-next-binding
data ProjectModel (v :: Type -> Type) = ProjectModel
  { notExistingIds :: [Var Project.Id v]
  , byUsername     :: Map Username (Map (Var Project.Id v) Project.Creatable)
  }
  deriving stock Generic

data UserModel = UserModel
  { byUsername     :: Map Username User.RawCreatable
  , byEmailAddress :: Map EmailAddress User.RawCreatable
  }
  deriving stock Generic

-- brittany-disable-next-binding
data AuthTokenModel (v :: Type -> Type) = AuthTokenModel
  { byUsername :: Map Text (Var AuthToken v)
  }
  deriving stock Generic

mapUsernameTokenProjectIdAList :: (  ( Username
                                    , Var AuthToken v
                                    , Var Project.Id v
                                    )
                                  -> a
                                  )
                               -> Model v
                               -> [a]
mapUsernameTokenProjectIdAList f model = model |> foldMapOf
  (#project . #byUsername . to Map.toList . folded)
  \(username, byProjectId) -> do
    token     <- model ^.. #authToken . #byUsername . at username . folded
    projectId <- Map.keys byProjectId
    pure $ f (username, token, projectId)

getUsernameTokenProjectIdAList :: Model v
                               -> [ ( Username
                                    , Var AuthToken v
                                    , Var Project.Id v
                                    )
                                  ]
getUsernameTokenProjectIdAList = mapUsernameTokenProjectIdAList identity
