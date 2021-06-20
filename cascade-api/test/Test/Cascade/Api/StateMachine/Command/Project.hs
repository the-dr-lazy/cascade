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

module Test.Cascade.Api.StateMachine.Command.Project
    ( commands
    ) where

import qualified Cascade.Api.Data.Project                                            as Project
import qualified Cascade.Api.Hedgehog.Gen.Id                                         as Gen
import           Control.Lens                                                        ( (%~) )
import           Control.Lens.Combinators                                            ( cons )
import           Hedgehog
import           Prelude                                                             hiding
    ( getAll )
import           Test.Cascade.Api.StateMachine.Command.Project.Create
import           Test.Cascade.Api.StateMachine.Command.Project.DeleteExistingById
import           Test.Cascade.Api.StateMachine.Command.Project.DeleteNotExistingById
import           Test.Cascade.Api.StateMachine.Command.Project.GetAll
import           Test.Cascade.Api.StateMachine.Command.Project.GetExistingById
import           Test.Cascade.Api.StateMachine.Command.Project.GetNotExistingById
import           Test.Cascade.Api.StateMachine.Command.Project.UpdateExistingById
import           Test.Cascade.Api.StateMachine.Command.Project.UpdateNotExistingById
import           Test.Cascade.Api.StateMachine.Model                                 ( Model )

commands :: MonadGen g => MonadIO m => MonadTest m => [Command g m Model]
commands =
  [ create
  , getAll
  , addNotExistingId
  , getExistingById
  , getNotExistingById
  , updateExistingById
  , updateNotExistingById
  , deleteExistingById
  , deleteNotExistingById
  ]


newtype AddNotExistingId (v :: Type -> Type)
  = AddNotExistingId Project.Id
  deriving stock (Show)

instance HTraversable AddNotExistingId where
  htraverse _ input = pure $ coerce input

addNotExistingId :: forall g m . MonadGen g => Applicative m => Command g m Model
addNotExistingId =
  let generator :: Model Symbolic -> Maybe (g (AddNotExistingId Symbolic))
      generator _ = Gen.id |> fmap AddNotExistingId |> Just

      execute :: AddNotExistingId Concrete -> m Project.Id
      execute (AddNotExistingId id) = pure id

      update :: Model v -> AddNotExistingId v -> Var Project.Id v -> Model v
      update model _ id = model |> #project . #notExistingIds %~ cons id
  in  Command generator execute [Update update]
