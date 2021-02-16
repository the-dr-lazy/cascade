{-|
Module      : Test.Cascade.Api.StateMachine.Command.Authentication
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Test.Cascade.Api.StateMachine.Command.Authentication where

commands :: MonadGen g => MonadIO m => [Command g m Model]
commands = []

newtype ValidLogin (v :: Type -> Type) = ValidLogin
  { credential :: Authenticated.RawCredential }
  deriving stock (Generic, Show)

newtype InvalidLogin (v :: Type -> Type) = InvalidLogin
  { credential :: Authenticated.RawCredential }
  deriving stock (Generic, Show)
