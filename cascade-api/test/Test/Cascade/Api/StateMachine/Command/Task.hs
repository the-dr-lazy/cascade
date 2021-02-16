module Test.Cascade.Api.StateMachine.Command.Task
  ( commands
  )
where

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
import qualified Cascade.Api.Network.TestClient.Api.Projects.Tasks
                                               as Cascade.Api.Projects.Tasks
import           Cascade.Api.Test.Prelude       ( )
import           Control.Lens                   ( (?~)
                                                , (^.)
                                                , at
                                                , to
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
commands = [createValid, createInvalid]

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
      update model (CreateValid _ creatable) id =
          model |> #task . #creatables . at id ?~ creatable
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
