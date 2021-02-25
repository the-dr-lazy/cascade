{-|
Module      : Test.Cascade.Api.StateMachine.Command.Task
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

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
                                                , (^..)
                                                , (%~)
                                                , at
                                                , to
                                                , folded
                                                , traversed
                                                , cons
                                                , sans
                                                , _Just
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
import           Data.Maybe                     ( fromMaybe )

commands
  :: MonadGen g
  => GenBase g ~ Identity => MonadIO m => MonadTest m => [Command g m Model]
commands =
  [ createValid
  , createValidForNonExistingProject
  , createInvalid
  , getByProjectId
  , addNotExistingId
  , getExisting
  , getNotExisting
  , updateExisting
  , updateExistingInvalid
  , updateNotExisting
  , deleteExisting
  , deleteNotExisting
  ]

-- brittany-disable-next-binding
data Create (v :: Type -> Type) = Create
  { projectId  :: Var Project.Id v
  , creatable  :: Task.RawCreatable
  }
  deriving stock (Generic, Show)

instance HTraversable Create where
  htraverse f Create {..} = Create <$> htraverse f projectId <*> pure creatable

createValid
  :: forall g m
   . MonadGen g
  => GenBase g ~ Identity => MonadIO m => MonadTest m => Command g m Model
createValid =
  let generator :: Model Symbolic -> Maybe (g (Create Symbolic))
      generator model = case model ^. #project . #creatables . to Map.keys of
        []         -> Nothing
        projectIds -> Just $ do
          projectId  <- Gen.element projectIds
          title      <- Gen.nonEmptyText 30 Valid
          deadlineAt <- FormattedOffsetDatetime <$> Gen.deadline Valid
          let creatable = Task.RawCreatable { .. }
          pure $ Create { .. }

      execute :: Create Concrete -> m Task.Id
      execute (Create projectId creatable) = do
        label "[Task/Create Valid]"

        response <- evalIO
          $ Cascade.Api.Projects.Tasks.create (concrete projectId) creatable

        Response.Created task <-
          (response ^. #responseBody)
          |> matchUnion @(Response.Created Task.Readable)
          |> evalMaybe

        let id = task ^. #id

        checkEqReadableRawCreatableTask task creatable

        response ^. #responseStatusCode . #statusCode === 201

        footnoteShow response

        pure id

      update :: Ord1 v => Model v -> Create v -> Var Task.Id v -> Model v
      update model (Create projectId creatable) id =
          model |> #task . #creatables . at projectId ?~ newByProjectId
         where
          byProjectId =
            model ^. #task . #creatables . at projectId |> fromMaybe Map.empty
          newByProjectId = byProjectId |> at id ?~ creatable
  in  Command generator execute [Update update]

createValidForNonExistingProject
  :: forall g m
   . MonadGen g
  => GenBase g ~ Identity => MonadIO m => MonadTest m => Command g m Model
createValidForNonExistingProject =
  let
    generator :: Model Symbolic -> Maybe (g (Create Symbolic))
    generator model = case model ^. #project . #notExistingIds of
      []         -> Nothing
      projectIds -> Just $ do
        projectId  <- Gen.element projectIds
        title      <- Gen.nonEmptyText 30 Valid
        deadlineAt <- FormattedOffsetDatetime <$> Gen.deadline Valid
        let creatable = Task.RawCreatable { .. }
        pure $ Create { .. }

    execute :: Create Concrete -> m Cascade.Api.Projects.Tasks.CreateResponse
    execute (Create projectId creatable) =
      evalIO $ Cascade.Api.Projects.Tasks.create (concrete projectId) creatable

    ensure
      :: Model Concrete
      -> Model Concrete
      -> Create Concrete
      -> Cascade.Api.Projects.Tasks.CreateResponse
      -> Test ()
    ensure _before _after _input response = do
      footnoteShow response

      response ^. #responseStatusCode . #statusCode === 404
  in
    Command generator execute [Ensure ensure]

createInvalid
  :: forall g m
   . MonadGen g
  => GenBase g ~ Identity => MonadIO m => MonadTest m => Command g m Model
createInvalid =
  let
    generator :: Model Symbolic -> Maybe (g (Create Symbolic))
    generator model = case model ^. #project . #creatables . to Map.keys of
      []         -> Nothing
      projectIds -> Just $ do
        flag <- Gen.bool_
        let boolToValidity     = bool Invalid Valid
        let titleValidity      = boolToValidity flag
        let deadlineAtValidity = boolToValidity $ not flag
        projectId  <- Gen.element projectIds
        title      <- Gen.nonEmptyText 30 titleValidity
        deadlineAt <- FormattedOffsetDatetime
          <$> Gen.deadline deadlineAtValidity
        let creatable = Task.RawCreatable { .. }
        pure $ Create { .. }

    coverage :: Create Concrete -> m ()
    coverage (Create _ creatable) = do
      let flag = creatable ^. #title . to Text.null
      cover 5 "invalid deadline" flag
      cover 5 "empty title"      (not flag)

    execute :: Create Concrete -> m Cascade.Api.Projects.Tasks.CreateResponse
    execute input@(Create projectId creatable) = do
      label "[Task/Create Invalid]"
      coverage input

      evalIO $ Cascade.Api.Projects.Tasks.create (concrete projectId) creatable

    ensure
      :: Model Concrete
      -> Model Concrete
      -> Create Concrete
      -> Cascade.Api.Projects.Tasks.CreateResponse
      -> Test ()
    ensure _before _after _input response = do
      footnoteShow response

      (response ^. #responseBody)
        |> matchUnion
           @(Response.Unprocessable Task.RawCreatableValidationErrors)
        |> evalMaybe

      response ^. #responseStatusCode . #statusCode === 422
  in
    Command generator execute [Ensure ensure]

-- brittany-disable-next-binding
data GetByProjectId (v :: Type -> Type) = GetByProjectId
  { projectId  :: Var Project.Id v
  }
  deriving stock (Generic, Show)

instance HTraversable GetByProjectId where
  htraverse f (GetByProjectId id) = GetByProjectId <$> htraverse f id

getByProjectId
  :: forall g m . MonadGen g => MonadIO m => MonadTest m => Command g m Model
getByProjectId =
  let generator :: Model Symbolic -> Maybe (g (GetByProjectId Symbolic))
      generator model = case model ^. #project . #creatables . to Map.keys of
        []         -> Nothing
        projectIds -> Just $ do
          projectId <- Gen.element projectIds
          pure $ GetByProjectId { .. }

      execute
        :: GetByProjectId Concrete
        -> m Cascade.Api.Projects.Tasks.GetByProjectIdResponse
      execute (GetByProjectId projectId) = evalIO
        $ Cascade.Api.Projects.Tasks.getByProjectId (concrete projectId)

      ensure
        :: Model Concrete
        -> Model Concrete
        -> GetByProjectId Concrete
        -> Cascade.Api.Projects.Tasks.GetByProjectIdResponse
        -> Test ()
      ensure _before after (GetByProjectId projectId) response = do
        footnoteShow response

        output :: [Task.Readable] <-
          (response ^. #responseBody)
          |> matchUnion @(Response.Ok [Task.Readable])
          |> coerce
          |> evalMaybe

        let projectIdCreatables =
              after ^.. #task . #creatables . at projectId . _Just . folded

        length output === length projectIdCreatables

        for_
          output
          \task -> do
            let id = task ^. #id

            task' :: Task.RawCreatable <-
              after
              ^.. #task
              .   #creatables
              .   folded
              .   at (Var $ Concrete id)
              .   _Just
              |>  listToMaybe
              |>  evalMaybe

            checkEqReadableRawCreatableTask task task'

      require :: Model Symbolic -> GetByProjectId Symbolic -> Bool
      require model (GetByProjectId projectId) =
          case model ^. #task . #creatables . at projectId of
            Nothing -> False
            Just _  -> True
  in  Command generator execute [Require require, Ensure ensure]

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
  let generator :: Model Symbolic -> Maybe (g (GetById Symbolic))
      generator model =
          case model ^.. #task . #creatables . folded . to Map.keys |> mconcat of
            []  -> Nothing
            ids -> Gen.element ids |> fmap GetById |> Just

      execute :: GetById Concrete -> m Cascade.Api.Tasks.GetByIdResponse
      execute (GetById id) = evalIO . Cascade.Api.Tasks.getById $ concrete id

      ensure
        :: Model Concrete
        -> Model Concrete
        -> GetById Concrete
        -> Cascade.Api.Tasks.GetByIdResponse
        -> Test ()
      ensure before _after (GetById taskId) response = do
        footnoteShow response

        task :: Task.Readable <-
          (response ^. #responseBody)
          |> matchUnion @(Response.Ok Task.Readable)
          |> coerce
          |> evalMaybe

        let id = task ^. #id

        concrete taskId === id

        task' :: Task.RawCreatable <-
          before
          ^.. #task
          .   #creatables
          .   folded
          .   at (Var $ Concrete id)
          .   _Just
          |>  listToMaybe
          |>  evalMaybe

        checkEqReadableRawCreatableTask task task'

      require :: Model Symbolic -> GetById Symbolic -> Bool
      require model (GetById id) =
          case
              model
              ^.. #task
              .   #creatables
              .   folded
              .   at id
              .   _Just
              |>  listToMaybe
            of
              Nothing -> False
              Just _  -> True
  in  Command generator execute [Require require, Ensure ensure]


getNotExisting
  :: forall g m . MonadGen g => MonadIO m => MonadTest m => Command g m Model
getNotExisting =
  let
    generator :: Model Symbolic -> Maybe (g (GetById Symbolic))
    generator model = case model ^. #task . #notExistingIds of
      []  -> Nothing
      ids -> Gen.element ids |> fmap GetById |> Just

    execute :: GetById Concrete -> m Cascade.Api.Tasks.GetByIdResponse
    execute input =
      evalIO . Cascade.Api.Tasks.getById $ input ^. #id . concreted

    ensure
      :: Model Concrete
      -> Model Concrete
      -> GetById Concrete
      -> Cascade.Api.Tasks.GetByIdResponse
      -> Test ()
    ensure _before _after _input response = do
      footnoteShow response
      response ^. #responseStatusCode . #statusCode === 404
  in
    Command generator execute [Ensure ensure]

-- brittany-disable-next-binding
data UpdateById (v :: Type -> Type) = UpdateById
  { id :: Var Task.Id v
  , updatable :: Task.RawUpdatable
  }
  deriving stock (Generic, Show)

instance HTraversable UpdateById where
  htraverse f (UpdateById {..}) =
    UpdateById <$> htraverse f id <*> pure updatable

updateExisting
  :: forall g m . MonadGen g => MonadIO m => MonadTest m => Command g m Model
updateExisting =
  let
    generator :: Model Symbolic -> Maybe (g (UpdateById Symbolic))
    generator model =
      case model ^.. #task . #creatables . folded . to Map.keys |> mconcat of
        []  -> Nothing
        ids -> Just $ do
          id         <- Gen.element ids
          title      <- Just <$> Gen.nonEmptyText 30 Valid
          deadlineAt <- Just . FormattedOffsetDatetime <$> Gen.deadline Valid
          let updatable = Task.RawUpdatable { .. }
          pure $ UpdateById { .. }

    execute :: UpdateById Concrete -> m Cascade.Api.Tasks.UpdateByIdResponse
    execute UpdateById { id, updatable } =
      evalIO $ Cascade.Api.Tasks.updateById (concrete id) updatable

    ensure
      :: Model Concrete
      -> Model Concrete
      -> UpdateById Concrete
      -> Cascade.Api.Tasks.UpdateByIdResponse
      -> Test ()
    ensure _before _after input response = do
      footnoteShow response

      let id = input ^. #id . concreted
      task :: Task.Readable <-
        (response ^. #responseBody)
        |> matchUnion @(Response.Ok Task.Readable)
        |> coerce
        |> evalMaybe
      task ^. #id === id

      response ^. #responseStatusCode . #statusCode === 200

    update
      :: Ord1 v
      => Model v
      -> UpdateById v
      -> Var Cascade.Api.Tasks.UpdateByIdResponse v
      -> Model v
    update model (UpdateById id updatable) _response =
      let creatable = updateCreatableTask updatable
      in  model |> #task . #creatables . traversed %~ Map.adjust creatable id

    require :: Model Symbolic -> UpdateById Symbolic -> Bool
    require model (UpdateById id _) =
      case
          model ^.. #task . #creatables . folded . at id . _Just |> listToMaybe
        of
          Nothing -> False
          Just _  -> True
  in
    Command generator execute [Require require, Update update, Ensure ensure]

updateExistingInvalid
  :: forall g m . MonadGen g => MonadIO m => MonadTest m => Command g m Model
updateExistingInvalid =
  let
    generator :: Model Symbolic -> Maybe (g (UpdateById Symbolic))
    generator model =
      case model ^.. #task . #creatables . folded . to Map.keys |> mconcat of
        []  -> Nothing
        ids -> Just $ do
          flag <- Gen.bool_
          let boolToValidity     = bool Invalid Valid
          let titleValidity      = boolToValidity flag
          let deadlineAtValidity = boolToValidity $ not flag
          id         <- Gen.element ids
          title      <- Just <$> Gen.nonEmptyText 30 titleValidity
          deadlineAt <-
            Just . FormattedOffsetDatetime <$> Gen.deadline deadlineAtValidity
          let updatable = Task.RawUpdatable { .. }
          pure $ UpdateById { .. }

    execute :: UpdateById Concrete -> m Cascade.Api.Tasks.UpdateByIdResponse
    execute UpdateById { id, updatable } =
      evalIO $ Cascade.Api.Tasks.updateById (concrete id) updatable

    ensure
      :: Model Concrete
      -> Model Concrete
      -> UpdateById Concrete
      -> Cascade.Api.Tasks.UpdateByIdResponse
      -> Test ()
    ensure _before _after _input response = do
      footnoteShow response

      (response ^. #responseBody)
        |> matchUnion
           @(Response.Unprocessable Task.RawUpdatableValidationErrors)
        |> evalMaybe

      response ^. #responseStatusCode . #statusCode === 422

    require :: Model Symbolic -> UpdateById Symbolic -> Bool
    require model (UpdateById id _) =
      case
          model ^.. #task . #creatables . folded . at id . _Just |> listToMaybe
        of
          Nothing -> False
          Just _  -> True
  in
    Command generator execute [Require require, Ensure ensure]

updateNotExisting
  :: forall g m . MonadGen g => MonadIO m => MonadTest m => Command g m Model
updateNotExisting =
  let
    generator :: Model Symbolic -> Maybe (g (UpdateById Symbolic))
    generator model = case model ^. #task . #notExistingIds of
      []  -> Nothing
      ids -> Just $ do
        id         <- Gen.element ids
        title      <- Just <$> Gen.nonEmptyText 30 Valid
        deadlineAt <- Just . FormattedOffsetDatetime <$> Gen.deadline Valid
        let updatable = Task.RawUpdatable { .. }
        pure $ UpdateById { .. }

    execute :: UpdateById Concrete -> m Cascade.Api.Tasks.UpdateByIdResponse
    execute input@UpdateById { updatable } =
      evalIO $ Cascade.Api.Tasks.updateById id updatable
      where id = input ^. #id . concreted

    ensure
      :: Model Concrete
      -> Model Concrete
      -> UpdateById Concrete
      -> Cascade.Api.Tasks.UpdateByIdResponse
      -> Test ()
    ensure _before _after _input response = do
      footnoteShow response
      response ^. #responseStatusCode . #statusCode === 404
  in
    Command generator execute [Ensure ensure]

-- brittany-disable-next-binding
newtype DeleteById (v :: Type -> Type) = DeleteById
  { id :: Var Task.Id v
  }
  deriving stock (Generic, Show)

instance HTraversable DeleteById where
  htraverse f (DeleteById id) = DeleteById <$> htraverse f id

deleteExisting
  :: forall g m . MonadGen g => MonadIO m => MonadTest m => Command g m Model
deleteExisting =
  let generator :: Model Symbolic -> Maybe (g (DeleteById Symbolic))
      generator model =
          case model ^.. #task . #creatables . folded . to Map.keys |> mconcat of
            []  -> Nothing
            ids -> Gen.element ids |> fmap DeleteById |> Just

      execute :: DeleteById Concrete -> m Cascade.Api.Tasks.DeleteByIdResponse
      execute input = evalIO . Cascade.Api.Tasks.deleteById $ id
          where id = input ^. #id . concreted

      ensure
        :: Model Concrete
        -> Model Concrete
        -> DeleteById Concrete
        -> Cascade.Api.Tasks.DeleteByIdResponse
        -> Test ()
      ensure _before _after input response = do
        footnoteShow response
        task :: Task.Readable <-
          (response ^. #responseBody)
          |> matchUnion @(Response.Ok Task.Readable)
          |> coerce
          |> evalMaybe
        task ^. #id === input ^. #id . concreted

      update
        :: Ord1 v
        => Model v
        -> DeleteById v
        -> Var Cascade.Api.Tasks.DeleteByIdResponse v
        -> Model v
      update model (DeleteById id) _response =
          model |> #task . #creatables . traversed %~ sans id

      require :: Model Symbolic -> DeleteById Symbolic -> Bool
      require model (DeleteById id) =
          case
              model
              ^.. #task
              .   #creatables
              .   folded
              .   at id
              .   _Just
              |>  listToMaybe
            of
              Nothing -> False
              Just _  -> True
  in  Command generator execute [Require require, Update update, Ensure ensure]

deleteNotExisting
  :: forall g m . MonadGen g => MonadIO m => MonadTest m => Command g m Model
deleteNotExisting =
  let generator :: Model Symbolic -> Maybe (g (DeleteById Symbolic))
      generator model = case model ^. #task . #notExistingIds of
        []  -> Nothing
        ids -> Gen.element ids |> fmap DeleteById |> Just

      execute :: DeleteById Concrete -> m Cascade.Api.Tasks.DeleteByIdResponse
      execute input = evalIO . Cascade.Api.Tasks.deleteById $ id
          where id = input ^. #id . concreted

      ensure
        :: Model Concrete
        -> Model Concrete
        -> DeleteById Concrete
        -> Cascade.Api.Tasks.DeleteByIdResponse
        -> Test ()
      ensure _before _after _input response = do
        footnoteShow response
        response ^. #responseStatusCode . #statusCode === 404
  in  Command generator execute [Ensure ensure]

updateCreatableTask
  :: Task.RawUpdatable -> Task.RawCreatable -> Task.RawCreatable
updateCreatableTask updatable Task.RawCreatable {..} = Task.RawCreatable
  { title      = fromMaybe title $ updatable ^. #title
  , deadlineAt = fromMaybe deadlineAt $ updatable ^. #deadlineAt
  }

checkEqReadableRawCreatableTask
  :: (MonadTest m, HasCallStack) => Task.Readable -> Task.RawCreatable -> m ()
checkEqReadableRawCreatableTask task creatable = do
  task ^. #title . to Text.NonEmpty.un === creatable ^. #title
  (task ^. #deadlineAt . to unFormattedOffsetDatetime . to offsetDatetimeToTime)
    === (  creatable
        ^. #deadlineAt
        .  to unFormattedOffsetDatetime
        .  to offsetDatetimeToTime
        )
