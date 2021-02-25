{-|
Module      : Test.Cascade.Api.StateMachine.Command.Project
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Test.Cascade.Api.StateMachine.Command.Project (commands) where

import qualified Cascade.Api.Data.Project           as Project
import qualified Cascade.Api.Hedgehog.Gen.Api.Project
                                                    as Gen
import qualified Cascade.Api.Hedgehog.Gen.Id        as Gen
import qualified Cascade.Api.Network.TestClient.Api.Projects
                                                    as Cascade.Api.Projects
import qualified Cascade.Api.Servant.Response       as Response
import           Cascade.Api.Test.Prelude

import           Control.Lens                        ( (%~)
                                                     , (?~)
                                                     , (^.)
                                                     , at
                                                     , sans
                                                     , to
                                                     )
import           Control.Lens.Combinators            ( cons )
import           Control.Monad.Managed
import           Data.Generics.Labels                ( )
import qualified Data.Map.Strict                    as Map
import           Hedgehog
import qualified Hedgehog.Gen                       as Gen
import           Prelude                      hiding ( getAll )
import           Servant.API.UVerb.Union             ( matchUnion )
import           Test.Cascade.Api.StateMachine.Model ( Model )

commands :: MonadGen g => MonadIO m => MonadTest m => [Command g m Model]
commands =
  [create, getAll, addNotExistingId, getExisting, getNotExisting, updateExisting, updateNotExisting, deleteExisting, deleteNotExisting]

-- brittany-disable-next-binding
newtype Create (v :: Type -> Type) = Create
  { creatable :: Project.Creatable
  }
  deriving stock (Generic, Show)

instance HTraversable Create where
  htraverse _ (Create creatable) = pure $ Create creatable

create :: forall g m . MonadGen g => MonadIO m => MonadTest m => Command g m Model
create =
  let generator :: Model Symbolic -> Maybe (g (Create Symbolic))
      generator _ = Gen.project |> fmap Create |> Just

      execute :: Create Concrete -> m Project.Id
      execute (Create creatable) = do
        response <- evalIO . Cascade.Api.Projects.create $ creatable
        footnoteShow response

        project <- (response ^. #responseBody) |> matchUnion @(Response.Created Project.Readable) |> coerce |> evalMaybe
        let id = project ^. #id

        project === mkReadableProjectFromCreatableProject id creatable

        pure id
  in  Command generator execute [Update \model (Create creatable) id -> model |> #project . #creatables . at id ?~ creatable]

-- brittany-disable-next-binding
data GetAll (v :: Type -> Type) = GetAll deriving stock Show

instance HTraversable GetAll where
  htraverse _ _ = pure GetAll

getAll :: forall g m . MonadGen g => MonadIO m => MonadTest m => Command g m Model
getAll =
  let generator :: Model Symbolic -> Maybe (g (GetAll Symbolic))
      generator _ = Just . pure $ GetAll

      execute :: GetAll Concrete -> m Cascade.Api.Projects.GetAllResponse
      execute _ = evalIO Cascade.Api.Projects.getAll
  in  Command
        generator
        execute
        [ Ensure \_before after _input response -> do
            footnoteShow response
            output :: [Project.Readable] <- (response ^. #responseBody) |> matchUnion @(Response.Ok [Project.Readable]) |> coerce |> evalMaybe

            length output === Map.size (after ^. #project . #creatables)

            for_
              output
              \project -> do
                let id = project ^. #id
                project' <-
                  (after ^. #project . #creatables . at (Var $ Concrete id)) |> fmap (mkReadableProjectFromCreatableProject id) |> evalMaybe
                project === project'
        ]

-- brittany-disable-next-binding
data AddNotExistingId (v :: Type -> Type) = AddNotExistingId
  { id :: Project.Id
  }
  deriving stock Show

instance HTraversable AddNotExistingId where
  htraverse _ (AddNotExistingId id) = pure $ AddNotExistingId id

addNotExistingId :: forall g m . MonadGen g => Applicative m => Command g m Model
addNotExistingId =
  let generator :: Model Symbolic -> Maybe (g (AddNotExistingId Symbolic))
      generator _ = Gen.id |> fmap AddNotExistingId |> Just

      execute :: AddNotExistingId Concrete -> m Project.Id
      execute (AddNotExistingId id) = pure id
  in  Command generator execute [Update \model _input id -> model |> #project . #notExistingIds %~ cons id]

-- brittany-disable-next-binding
newtype GetById (v :: Type -> Type) = GetById
  { id :: Var Project.Id v
  }
  deriving stock (Generic, Show)

instance HTraversable GetById where
  htraverse f (GetById id) = GetById <$> htraverse f id

getExisting :: forall g m . MonadGen g => MonadIO m => MonadTest m => Command g m Model
getExisting =
  let
    generator :: Model Symbolic -> Maybe (g (GetById Symbolic))
    generator model = case model ^. #project . #creatables . to Map.keys of
      []  -> Nothing
      ids -> Gen.element ids |> fmap GetById |> Just

    execute :: GetById Concrete -> m Cascade.Api.Projects.GetByIdResponse
    execute input = evalIO . Cascade.Api.Projects.getById $ input ^. #id . concreted
  in
    Command
      generator
      execute
      [ Ensure \before _after _input response -> do
          footnoteShow response
          project :: Project.Readable <- (response ^. #responseBody) |> matchUnion @(Response.Ok Project.Readable) |> coerce |> evalMaybe

          let id = project ^. #id

          project' <-
            (before ^. #project . #creatables)
            |> Map.lookup (Var $ Concrete id)
            |> fmap (mkReadableProjectFromCreatableProject id)
            |> evalMaybe
          project === project'
      ]

getNotExisting :: forall g m . MonadGen g => MonadIO m => MonadTest m => Command g m Model
getNotExisting =
  let generator :: Model Symbolic -> Maybe (g (GetById Symbolic))
      generator model = case model ^. #project . #notExistingIds of
        []  -> Nothing
        ids -> Gen.element ids |> fmap GetById |> Just

      execute :: GetById Concrete -> m Cascade.Api.Projects.GetByIdResponse
      execute input = evalIO . Cascade.Api.Projects.getById $ input ^. #id . concreted
  in  Command
        generator
        execute
        [ Ensure \_before _after _input response -> do
            footnoteShow response
            response ^. #responseStatusCode . #statusCode === 404
        ]

-- brittany-disable-next-binding
data UpdateById (v :: Type -> Type) = UpdateById
  { id :: Var Project.Id v
  , updatable :: Project.Updatable
  }
  deriving stock (Generic, Show)

instance HTraversable UpdateById where
  htraverse f (UpdateById {..}) = UpdateById <$> htraverse f id <*> pure updatable

updateExisting :: forall g m . MonadGen g => MonadIO m => MonadTest m => Command g m Model
updateExisting =
  let generator :: Model Symbolic -> Maybe (g (UpdateById Symbolic))
      generator model = case model ^. #project . #creatables . to Map.keys of
        []  -> Nothing
        ids -> Just $ UpdateById <$> Gen.element ids <*> Gen.project

      execute :: UpdateById Concrete -> m Project.Id
      execute input@UpdateById { updatable } = do
        let id = input ^. #id . concreted
        response <- evalIO $ Cascade.Api.Projects.updateById id updatable
        footnoteShow response
        project :: Project.Readable <- (response ^. #responseBody) |> matchUnion @(Response.Ok Project.Readable) |> coerce |> evalMaybe
        project ^. #id === id

        pure id
  in  Command
        generator
        execute
        [ Update \model UpdateById { updatable } id ->
            let creatable = updateCreatableProject updatable in model |> #project . #creatables %~ Map.adjust creatable id
        ]

updateNotExisting :: forall g m . MonadGen g => MonadIO m => MonadTest m => Command g m Model
updateNotExisting =
  let generator :: Model Symbolic -> Maybe (g (UpdateById Symbolic))
      generator model = case model ^. #project . #notExistingIds of
        []  -> Nothing
        ids -> Just $ UpdateById <$> Gen.element ids <*> Gen.project

      execute :: UpdateById Concrete -> m Cascade.Api.Projects.UpdateByIdResponse
      execute input@UpdateById { updatable } = evalIO $ Cascade.Api.Projects.updateById id updatable where id = input ^. #id . concreted
  in  Command
        generator
        execute
        [ Ensure \_before _after _input response -> do
            footnoteShow response
            response ^. #responseStatusCode . #statusCode === 404
        ]

-- brittany-disable-next-binding
newtype DeleteById (v :: Type -> Type) = DeleteById
  { id :: Var Project.Id v
  }
  deriving stock (Generic, Show)

instance HTraversable DeleteById where
  htraverse f (DeleteById id) = DeleteById <$> htraverse f id

deleteExisting :: forall g m . MonadGen g => MonadIO m => MonadTest m => Command g m Model
deleteExisting =
  let generator :: Model Symbolic -> Maybe (g (DeleteById Symbolic))
      generator model = case model ^. #project . #creatables . to Map.keys of
        []  -> Nothing
        ids -> Gen.element ids |> fmap DeleteById |> Just

      execute :: DeleteById Concrete -> m Cascade.Api.Projects.DeleteByIdResponse
      execute input = evalIO . Cascade.Api.Projects.deleteById $ id where id = input ^. #id . concreted
  in  Command
        generator
        execute
        [ Update \model (DeleteById id) _output -> model |> #project . #creatables %~ sans id
        , Ensure \_before _after input response -> do
          footnoteShow response
          project :: Project.Readable <- (response ^. #responseBody) |> matchUnion @(Response.Ok Project.Readable) |> coerce |> evalMaybe
          project ^. #id === input ^. #id . concreted
        ]

deleteNotExisting :: forall g m . MonadGen g => MonadIO m => MonadTest m => Command g m Model
deleteNotExisting =
  let generator :: Model Symbolic -> Maybe (g (DeleteById Symbolic))
      generator model = case model ^. #project . #notExistingIds of
        []  -> Nothing
        ids -> Gen.element ids |> fmap DeleteById |> Just

      execute :: DeleteById Concrete -> m Cascade.Api.Projects.DeleteByIdResponse
      execute input = evalIO . Cascade.Api.Projects.deleteById $ id where id = input ^. #id . concreted
  in  Command
        generator
        execute
        [ Ensure \_before _after _input response -> do
            footnoteShow response
            response ^. #responseStatusCode . #statusCode === 404
        ]

mkReadableProjectFromCreatableProject :: Project.Id -> Project.Creatable -> Project.Readable
mkReadableProjectFromCreatableProject id Project.Creatable {..} = Project.Readable { .. }

updateCreatableProject :: Project.Updatable -> Project.Creatable -> Project.Creatable
updateCreatableProject updatable Project.Creatable {..} = Project.Creatable { name = fromMaybe name $ updatable ^. #name }
