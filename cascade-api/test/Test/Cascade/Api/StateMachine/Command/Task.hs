module Test.Cascade.Api.StateMachine.Command.Task
  ( commands
  )
where

import           Cascade.Api.Test.Prelude
import qualified Cascade.Api.Data.Project      as Project
import qualified Cascade.Api.Data.Task         as Task
import           Cascade.Api.Data.OffsetDatetime
                                                ( FormattedOffsetDatetime(..)
                                                , unFormattedOffsetDatetime
                                                )
import           Cascade.Api.Hedgehog.Gen.Prelude
import qualified Cascade.Api.Hedgehog.Gen.Text as Gen
import qualified Cascade.Api.Hedgehog.Gen.Chronos
                                               as Gen
import qualified Cascade.Api.Hedgehog.Gen.Id   as Gen
import qualified Cascade.Api.Network.TestClient.Api.Projects.Tasks
                                               as Cascade.Api.Projects.Tasks
import qualified Cascade.Api.Network.TestClient.Api.Tasks
                                               as Cascade.Api.Tasks
import           Cascade.Api.Test.Prelude       ( )
import           Control.Lens                   ( (?~)
                                                , (^.)
                                                , (%~)
                                                , at
                                                , to
                                                , filtered
                                                , folded
                                                , cons
                                                )
import           Servant.API.UVerb.Union        ( matchUnion )
import qualified Data.Map                      as Map
import qualified Cascade.Data.Text             as Text
import qualified Cascade.Data.Text.NonEmpty    as Text.NonEmpty
import qualified Cascade.Data.Chronos.Future   as Future
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import           Test.Cascade.Api.StateMachine.Model
                                                ( Model )
import qualified Cascade.Api.Servant.Response  as Response
import           Validation                     ( Validation(..) )
import           Chronos                        ( offsetDatetimeToTime )


commands
  :: MonadGen g
  => GenBase g ~ Identity => MonadIO m => MonadTest m => [Command g m Model]
commands = [createValid, createInvalid, findByProjectId, addNotExistingId, getExisting]

-- brittany-disable-next-binding
data CreateValid (v :: Type -> Type) = CreateValid
  { projectId  :: Var Project.Id v
  , creatable  :: Task.RawCreatable
  }
  deriving stock (Generic, Show)

instance HTraversable CreateValid where
  htraverse f CreateValid {..} =
    CreateValid <$> htraverse f projectId <*> pure creatable

createValid
  :: forall g m
   . MonadGen g
  => GenBase g ~ Identity => MonadIO m => MonadTest m => Command g m Model
createValid =
  let generator :: Model Symbolic -> Maybe (g (CreateValid Symbolic))
      generator model = case model ^. #project . #creatables . to Map.keys of
        []         -> Nothing
        projectIds -> Just $ do
          projectId  <- Gen.element projectIds
          title      <- Gen.nonEmptyText Valid
          deadlineAt <- FormattedOffsetDatetime <$> Gen.deadline Valid
          let creatable = Task.RawCreatable { .. }
          pure $ CreateValid { .. }

      execute :: CreateValid Concrete -> m Task.Id
      execute (CreateValid projectId creatable) = do
        label "[Task/Create Valid]"

        response <- evalIO
          $ Cascade.Api.Projects.Tasks.create (concrete projectId) creatable

        Response.Created task <-
          (response ^. #responseBody)
          |> matchUnion @(Response.Created Task.Readable)
          |> evalMaybe

        let id = task ^. #id

        task ^. #title . to Text.NonEmpty.un === creatable ^. #title
        task
          ^.  #deadlineAt
          .   to unFormattedOffsetDatetime
          .   to offsetDatetimeToTime
          === creatable
          ^.  #deadlineAt
          .   to unFormattedOffsetDatetime
          .   to offsetDatetimeToTime

        response ^. #responseStatusCode . #statusCode === 201

        footnoteShow response

        pure id

      update :: Ord1 v => Model v -> CreateValid v -> Var Task.Id v -> Model v
      update model (CreateValid projectId creatable) id =
          model
            |> #task
            .  #creatables
            .  at id
            ?~ creatable
            |> #task
            .  #byProjectId
            .  at projectId
            %~ updateCreatables
         where
          updateCreatables
            :: Maybe [Task.RawCreatable] -> Maybe [Task.RawCreatable]
          updateCreatables Nothing           = Just [creatable]
          updateCreatables (Just creatables) = Just $ creatables <> [creatable]
  in  Command generator execute [Update update]


-- brittany-disable-next-binding
data CreateInvalid (v :: Type -> Type) = CreateInvalid
  { titleValidity :: Validity
  , projectId  :: Var Project.Id v
  , creatable  :: Task.RawCreatable
  }
  deriving stock (Generic, Show)

instance HTraversable CreateInvalid where
  htraverse f CreateInvalid {..} =
    CreateInvalid
      <$> pure titleValidity
      <*> htraverse f projectId
      <*> pure creatable

createInvalid
  :: forall g m
   . MonadGen g
  => GenBase g ~ Identity => MonadIO m => MonadTest m => Command g m Model
createInvalid =
  let
    generator :: Model Symbolic -> Maybe (g (CreateInvalid Symbolic))
    generator model = case model ^. #project . #creatables . to Map.keys of
      []         -> Nothing
      projectIds -> Just $ do
        flag <- Gen.bool_
        let boolToValidity     = bool Invalid Valid
        let titleValidity      = boolToValidity flag
        let deadlineAtValidity = boolToValidity $ not flag
        projectId  <- Gen.element projectIds
        title      <- Gen.nonEmptyText titleValidity
        deadlineAt <- FormattedOffsetDatetime
          <$> Gen.deadline deadlineAtValidity
        let creatable = Task.RawCreatable { .. }
        pure $ CreateInvalid { .. }


    coverage :: CreateInvalid Concrete -> m ()
    coverage (CreateInvalid titleValidity _ _) = do
      let flag = validityToBool titleValidity
      cover 5 "invalid deadline" flag
      cover 5 "empty title"      (not flag)

    execute :: CreateInvalid Concrete -> m ()
    execute input@(CreateInvalid titleValidity projectId creatable) = do
      label "[Task/Create Invalid]"
      coverage input

      response <- evalIO
        $ Cascade.Api.Projects.Tasks.create (concrete projectId) creatable

      (response ^. #responseBody)
        |> matchUnion
           @(Response.Unprocessable Task.RawCreatableValidationErrors)
        |> evalMaybe

      response ^. #responseStatusCode . #statusCode === 422

      footnoteShow response

      pure ()

    update :: Model v -> CreateInvalid v -> Var () v -> Model v
    update model _ _ = model
  in
    Command generator execute [Update update]


validityToBool :: Validity -> Bool
validityToBool Valid   = True
validityToBool Invalid = False


-- brittany-disable-next-binding
data FindByProjectId (v :: Type -> Type) = FindByProjectId
  { projectId  :: Var Project.Id v
  }
  deriving stock (Generic, Show)

instance HTraversable FindByProjectId where
  htraverse f (FindByProjectId id) = FindByProjectId <$> htraverse f id

findByProjectId
  :: forall g m . MonadGen g => MonadIO m => MonadTest m => Command g m Model
findByProjectId =
  let
    generator :: Model Symbolic -> Maybe (g (FindByProjectId Symbolic))
    generator model = case model ^. #project . #creatables . to Map.keys of
      []         -> Nothing
      projectIds -> Just $ do
        projectId <- Gen.element projectIds
        pure $ FindByProjectId { .. }

    execute
      :: FindByProjectId Concrete
      -> m Cascade.Api.Projects.Tasks.FindByProjectIdResponse
    execute (FindByProjectId projectId) = evalIO
      $ Cascade.Api.Projects.Tasks.findByProjectId (concrete projectId)

    ensure
      :: Model Concrete
      -> Model Concrete
      -> FindByProjectId Concrete
      -> Cascade.Api.Projects.Tasks.FindByProjectIdResponse
      -> Test ()
    ensure _before after (FindByProjectId projectId) response = do
      footnoteShow response

      output :: [Task.Readable] <-
        (response ^. #responseBody)
        |> matchUnion @(Response.Ok [Task.Readable])
        |> coerce
        |> evalMaybe

      length output
        === maybe 0 length (after ^. #task . #byProjectId . at projectId)

      for_
        output
        \task -> do
          let id = task ^. #id

          task' :: Task.RawCreatable <-
            (after ^. #task . #creatables . at (Var $ Concrete id))
              |> evalMaybe

          task ^. #title . to Text.NonEmpty.un === task' ^. #title
          task
            ^.  #deadlineAt
            .   to unFormattedOffsetDatetime
            .   to offsetDatetimeToTime
            === task'
            ^.  #deadlineAt
            .   to unFormattedOffsetDatetime
            .   to offsetDatetimeToTime
  in
    Command generator execute [Ensure ensure]


-- brittany-disable-next-binding
data AddNotExistingId (v :: Type -> Type) = AddNotExistingId
  { id :: Task.Id
  }
  deriving stock Show

instance HTraversable AddNotExistingId where
  htraverse _ (AddNotExistingId id) = pure $ AddNotExistingId id

addNotExistingId
  :: forall g m . MonadGen g => Applicative m => Command g m Model
addNotExistingId =
  let generator :: Model Symbolic -> Maybe (g (AddNotExistingId Symbolic))
      generator _ = Gen.id |> fmap AddNotExistingId |> Just

      execute :: AddNotExistingId Concrete -> m Task.Id
      execute (AddNotExistingId id) = pure id

      update :: Model v -> AddNotExistingId v -> Var Task.Id v -> Model v
      update model _input id = model |> #task . #notExistingIds %~ cons id
  in  Command generator execute [Update update]

-- brittany-disable-next-binding
newtype GetById (v :: Type -> Type) = GetById
  { id :: Var Task.Id v
  }
  deriving stock (Generic, Show)

instance HTraversable GetById where
  htraverse f (GetById id) = GetById <$> htraverse f id

getExisting
  :: forall g m . MonadGen g => MonadIO m => MonadTest m => Command g m Model
getExisting =
  let
    generator :: Model Symbolic -> Maybe (g (GetById Symbolic))
    generator model = case model ^. #task . #creatables . to Map.keys of
      []  -> Nothing
      ids -> Gen.element ids |> fmap GetById |> Just

    execute :: GetById Concrete -> m Cascade.Api.Tasks.GetByIdResponse
    execute (GetById id) =
      evalIO . Cascade.Api.Tasks.getById $ concrete id

    ensure
      :: Model Concrete
      -> Model Concrete
      -> GetById Concrete
      -> Cascade.Api.Tasks.GetByIdResponse
      -> Test ()
    ensure before _after (GetById taskId) response = do
      footnoteShow "Hello from get by id"
      footnoteShow response
      footnoteShow taskId
      footnoteShow $ before ^. #task . #creatables
      footnoteShow $ before ^. #task . #byProjectId

      task :: Task.Readable <-
        (response ^. #responseBody)
        |> matchUnion @(Response.Ok Task.Readable)
        |> coerce
        |> evalMaybe

      pure ()

      -- let id = task ^. #id

      -- concrete tasksId === id

      -- task' :: Task.RawCreatable <-
      --   (before ^. #task . #creatables . at (Var $ Concrete id)) |> evalMaybe

      -- task ^. #title . to Text.NonEmpty.un === task' ^. #title
      -- task
      --   ^.  #deadlineAt
      --   .   to unFormattedOffsetDatetime
      --   .   to offsetDatetimeToTime
      --   === task'
      --   ^.  #deadlineAt
      --   .   to unFormattedOffsetDatetime
      --   .   to offsetDatetimeToTime
  in
    Command generator execute [Ensure ensure]
