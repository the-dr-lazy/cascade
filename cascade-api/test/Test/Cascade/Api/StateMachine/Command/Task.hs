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
import qualified Cascade.Data.Char             as Char
import           Control.Lens                   ( (?~)
                                                , (^.)
                                                , _2
                                                , at
                                                , to
                                                , view
                                                )
import           Servant.API.UVerb.Union        ( matchUnion )
import qualified Data.Map                      as Map
import qualified Cascade.Data.Text             as Text
import qualified Cascade.Data.Text.NonEmpty    as Text.NonEmpty
import qualified Cascade.Data.Chronos.Future as Future
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import           Test.Cascade.Api.StateMachine.Model
                                                ( Model )
import qualified Cascade.Api.Servant.Response  as Response
import           Cascade.Api.Servant.Response   ( Created(..) )
import           Validation                     ( Validation(..) )
import Chronos (now)


commands
  :: MonadGen g
  => GenBase g ~ Identity => MonadIO m => MonadTest m => [Command g m Model]
commands = [create]

-- brittany-disable-next-binding
data Create (v :: Type -> Type) = Create
  { titleValidity   :: Validity
  , projectId  :: Var Project.Id v
  , creatable  :: Task.RawCreatable
  }
  deriving stock (Generic, Show)

instance HTraversable Create where
  htraverse f Create {..} = Create <$> pure titleValidity <*> htraverse f projectId <*> pure creatable

create
  :: forall g m
   . MonadGen g
  => GenBase g ~ Identity => MonadIO m => MonadTest m => Command g m Model
create =
  let generator :: Model Symbolic -> Maybe (g (Create Symbolic))
      generator model = case model ^. #project . #creatables . to Map.keys of
        []         -> Nothing
        projectIds -> Just $ do
          projectId <- Gen.element projectIds
          (titleValidity, title) <- Gen.nonEmptyTextWithValidity
          deadlineAt <- FormattedOffsetDatetime <$> Gen.offsetDateTime
          let creatable = Task.RawCreatable { .. }
          pure $ Create { .. }

      execute :: Create Concrete -> m ()
      execute input@(Create titleValidity projectId creatable) = do
        label "[Task/Create]"
        -- coverage input
        response <- evalIO
          $ Cascade.Api.Projects.Tasks.create (concrete projectId) creatable

        -- task <-
        --   (response ^. #responseBody)
        --   |> matchUnion @(Response.Created Task.Readable)
        --   |> coerce
        --   |> evalMaybe

        -- let id = task ^. #id

        -- footnoteShow response

        -- let statusCode = response ^. #responseStatusCode . #statusCode
        -- case input of
        --   Create Valid   _ _ -> statusCode === 201
        --   Create Invalid _ _ -> statusCode === 422

        return ()

      -- update
      --   :: Model v
      --   -> Create v
      --   -> Var Task.Id v
      --   -> Model v
      update model (Create Invalid _ _) _ = model
      update model (Create Valid projectId creatable) id = model
          -- model |> #task . #creatables . at id ?~ creatable
  in  Command generator execute [Update update]

