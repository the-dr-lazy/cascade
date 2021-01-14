module Cascade.Test.Network.Server.Api.Projects
  ( hprop_projectsEndpoint
  ) where

import           Cascade.Data.Api.Project
import qualified Cascade.Data.Api.Project      as Project
import qualified Cascade.Hedgehog.Gen          as Gen
import qualified Cascade.Hedgehog.Gen.Api.Project
                                               as Gen
import qualified Cascade.Network.TestClient.Api.Projects
                                               as Cascade.Project
import qualified Cascade.Servant.Resource      as Resource
import           Cascade.Test.Prelude
import           Control.Lens                   ( (%~)
                                                , (^.)
                                                )
import           Control.Lens.Combinators       ( cons )
import           Data.Generics.Labels           ( )
import           Data.Generics.Labels           ( )
import qualified Data.Map.Strict               as Map
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Servant.API.UVerb.Union        ( matchUnion )

-- brittany-disable-next-binding
data Model (v :: Type -> Type) = Model
  { projects       :: Map (Var Project.Id v) (Creatable Project)
  , notExistingIds :: [Var Project.Id v]
  }
  deriving stock Generic

-- brittany-disable-next-binding
newtype Create (v :: Type -> Type) = Create
  { creatable :: Creatable Project
  }
  deriving stock (Generic, Show)

instance HTraversable Create where
  htraverse _ (Create projects) = pure $ Create projects



c_createProject :: forall g m
                 . MonadGen g
                => MonadIO m => MonadTest m => Command g m Model
c_createProject =
  let generator :: Model Symbolic -> Maybe (g (Create Symbolic))
      generator _ = Gen.project |> fmap Create |> Just

      execute :: Create Concrete -> m Project.Id
      execute (Create creatable) = do
        response <- evalIO . Cascade.Project.create $ creatable
        let project = response ^. #responseBody
            id      = project ^. #id

        response ^. #responseStatusCode . #statusCode === 201
        project === mkReadableProjectFromCreatableProject id creatable

        pure id
  in  Command
        generator
        execute
        [ Update \model (Create creatable) id ->
            model |> #projects %~ Map.insert id creatable
        ]

-- brittany-disable-next-binding
data GetAll (v :: Type -> Type) = GetAll deriving stock Show

instance HTraversable GetAll where
  htraverse _ _ = pure GetAll

c_getAllProjects :: forall g m
                  . MonadGen g
                 => MonadIO m => MonadTest m => Command g m Model
c_getAllProjects =
  let generator :: Model Symbolic -> Maybe (g (GetAll Symbolic))
      generator _ = Just . pure $ GetAll

      execute :: GetAll Concrete -> m [Readable Project]
      execute _ = do
        response <- evalIO Cascade.Project.getAll

        response ^. #responseStatusCode . #statusCode === 200

        pure $ response ^. #responseBody
  in  Command
        generator
        execute
        [ Ensure \Model { projects } _after _input output -> do
            length projects === length output
            for_
              output
              \project -> do
                let id = project ^. #id
                project' <-
                  Map.lookup (Var $ Concrete id) projects
                  |> fmap (mkReadableProjectFromCreatableProject id)
                  |> evalMaybe
                project === project'
        ]

-- brittany-disable-next-binding
data AddNotExistingId (v :: Type -> Type) = AddNotExistingId
  { id :: Project.Id
  }
  deriving stock Show

instance HTraversable AddNotExistingId where
  htraverse _ (AddNotExistingId id) = pure $ AddNotExistingId id

c_addNotExistingId :: forall g m
                    . MonadGen g
                   => Applicative m => Command g m Model
c_addNotExistingId =
  let generator :: Model Symbolic -> Maybe (g (AddNotExistingId Symbolic))
      generator _ =
        Gen.uuid |> fmap Project.Id |> fmap AddNotExistingId |> Just

      execute :: AddNotExistingId Concrete -> m Project.Id
      execute (AddNotExistingId id) = pure id
  in  Command generator
              execute
              [Update \model _input id -> model |> #notExistingIds %~ cons id]

-- brittany-disable-next-binding
newtype GetById (v :: Type -> Type) = GetById
  { id :: Var Project.Id v
  }
  deriving stock (Generic, Show)

instance HTraversable GetById where
  htraverse f (GetById id) = GetById <$> htraverse f id

c_getExistingProject :: forall g m
                      . MonadGen g
                     => MonadIO m => MonadTest m => Command g m Model
c_getExistingProject =
  let generator :: Model Symbolic -> Maybe (g (GetById Symbolic))
      generator Model { projects } = case Map.keys projects of
        []  -> Nothing
        ids -> Gen.element ids |> fmap GetById |> Just

      execute :: GetById Concrete -> m (Readable Project)
      execute input = do
        response <- evalIO . Cascade.Project.getById $ input ^. #id . concreted

        (response ^. #responseBody)
          |> matchUnion @(Resource.Ok (Readable Project))
          |> fmap coerce
          |> evalMaybe
  in  Command
        generator
        execute
        [ Ensure \Model { projects } _after _input project -> do
            let id = project ^. #id
            project' <-
              Map.lookup (Var $ Concrete id) projects
              |> fmap (mkReadableProjectFromCreatableProject id)
              |> evalMaybe
            project === project'
        ]

c_getNotExistingProject :: forall g m
                         . MonadGen g
                        => MonadIO m => MonadTest m => Command g m Model
c_getNotExistingProject =
  let generator :: Model Symbolic -> Maybe (g (GetById Symbolic))
      generator Model { notExistingIds } = case notExistingIds of
        []  -> Nothing
        ids -> Gen.element ids |> fmap GetById |> Just

      execute :: GetById Concrete -> m ()
      execute input = do
        response <- evalIO . Cascade.Project.getById $ input ^. #id . concreted
        response ^. #responseStatusCode . #statusCode === 404
  in  Command generator execute []

-- brittany-disable-next-binding
data UpdateById (v :: Type -> Type) = UpdateById
  { id :: Var Project.Id v
  , updatable :: Updatable Project
  }
  deriving stock (Generic, Show)

instance HTraversable UpdateById where
  htraverse f (UpdateById {..}) =
    UpdateById <$> htraverse f id <*> pure updatable

c_updateExistingProject :: forall g m
                         . MonadGen g
                        => MonadIO m => MonadTest m => Command g m Model
c_updateExistingProject =
  let generator :: Model Symbolic -> Maybe (g (UpdateById Symbolic))
      generator Model { projects } = case Map.keys projects of
        []  -> Nothing
        ids -> Just $ UpdateById <$> Gen.element ids <*> Gen.project

      execute :: UpdateById Concrete -> m Project.Id
      execute input@UpdateById { updatable } = do
        let id = input ^. #id . concreted
        response <- evalIO $ Cascade.Project.updateById id updatable
        let project = response ^. #responseBody

        project :: Readable Project <-
          (response ^. #responseBody)
          |> matchUnion @(Resource.Ok (Readable Project))
          |> fmap coerce
          |> evalMaybe

        project ^. #id === id

        pure id
  in  Command
        generator
        execute
        [ Update \model UpdateById { updatable } id ->
            model
              |> #projects
              %~ Map.adjust (updateCreatableProject updatable) id
        ]



c_updateNotExistingProject :: forall g m
                            . MonadGen g
                           => MonadIO m => MonadTest m => Command g m Model
c_updateNotExistingProject =
  let generator :: Model Symbolic -> Maybe (g (UpdateById Symbolic))
      generator Model { notExistingIds } = case notExistingIds of
        [] -> Nothing
        ids ->
          Just $ UpdateById <$> Gen.element notExistingIds <*> Gen.project

      execute :: UpdateById Concrete -> m ()
      execute input@UpdateById { updatable } = do
        let id = input ^. #id . concreted
        response <- evalIO $ Cascade.Project.updateById id updatable

        response ^. #responseStatusCode . #statusCode === 404
  in  Command generator execute [] -- ensure 404

-- brittany-disable-next-binding
newtype DeleteById (v :: Type -> Type) = DeleteById
  { id :: Var Project.Id v
  }
  deriving stock (Generic, Show)

instance HTraversable DeleteById where
  htraverse f (DeleteById id) = DeleteById <$> htraverse f id

c_deleteExistingProject :: forall g m
                         . MonadGen g
                        => MonadIO m => MonadTest m => Command g m Model
c_deleteExistingProject =
  let generator :: Model Symbolic -> Maybe (g (DeleteById Symbolic))
      generator Model { projects } = case Map.keys projects of
        []  -> Nothing
        ids -> Gen.element ids |> fmap DeleteById |> Just

      execute :: DeleteById Concrete -> m Project.Id
      execute input = do
        let id = input ^. #id . concreted

        response                    <- evalIO . Cascade.Project.deleteById $ id

        project :: Readable Project <-
          (response ^. #responseBody)
          |> matchUnion @(Resource.Ok (Readable Project))
          |> fmap coerce
          |> evalMaybe
        project ^. #id === id

        pure id
  in  Command generator
              execute
              [Update \model _input id -> model |> #projects %~ Map.delete id]

c_deleteNotExistingProject :: forall g m
                            . MonadGen g
                           => MonadIO m => MonadTest m => Command g m Model
c_deleteNotExistingProject =
  let generator :: Model Symbolic -> Maybe (g (DeleteById Symbolic))
      generator Model { notExistingIds } = case notExistingIds of
        []  -> Nothing
        ids -> Gen.element ids |> fmap DeleteById |> Just

      execute :: DeleteById Concrete -> m ()
      execute input = do
        let id = input ^. #id . concreted

        response <- evalIO . Cascade.Project.deleteById $ id

        response ^. #responseStatusCode . #statusCode === 404
  in  Command generator execute [] -- ensure 404

hprop_projectsEndpoint :: Property
hprop_projectsEndpoint = property do
  actions <- forAll $ Gen.sequential (Range.linear 1 100) initialModel commands
  executeSequential initialModel actions
 where
  initialModel = Model { projects = Map.empty, notExistingIds = mempty }
  commands =
    [ c_createProject
    , c_getAllProjects
    , c_addNotExistingId
    , c_getExistingProject
    , c_getNotExistingProject
    , c_updateExistingProject
    , c_updateNotExistingProject
    , c_deleteExistingProject
    , c_deleteNotExistingProject
    ]


mkReadableProjectFromCreatableProject :: Project.Id
                                      -> Creatable Project
                                      -> Readable Project
mkReadableProjectFromCreatableProject id (ProjectC {..}) = ProjectR { .. }

updateCreatableProject :: Updatable Project
                       -> Creatable Project
                       -> Creatable Project
updateCreatableProject updatable ProjectC {..} =
  ProjectC { name = fromMaybe name $ updatable ^. #name }
