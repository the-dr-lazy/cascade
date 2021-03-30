{-|
Module      : Cascade.CLI.Options
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

{-# LANGUAGE UndecidableInstances #-}

module Cascade.CLI.Options
  ( OptionsP(..)
  , PostgresOptionsP(..)
  , PartialOptions
  , PostgresPartialOptions
  , Options
  , PostgresOptions
  , mkOptions
  ) where

import           Generic.Data                        ( gmappend
                                                     , gmempty
                                                     )

data Phase = Partial | Final

infixl 3 :-
type family phase :- field where
  'Partial :- field = Last field
  'Final :- field = field

-- brittany-disable-next-binding
data PostgresOptionsP (p :: Phase) = PostgresOptions
  { host     :: p :- String
  , port     :: p :- Word16
  , user     :: p :- String
  , password :: p :- String
  , database :: p :- String
  } deriving stock Generic

type PostgresPartialOptions = PostgresOptionsP 'Partial

type PostgresOptions = PostgresOptionsP 'Final

-- brittany-disable-next-binding
data OptionsP (p :: Phase) = Options
  { httpPort        :: p :- Int
  , postgresOptions :: PostgresOptionsP p
  } deriving stock Generic

type PartialOptions = OptionsP 'Partial

type Options = OptionsP 'Final

instance Semigroup PostgresPartialOptions where
  (<>) = gmappend

instance Monoid PostgresPartialOptions where
  mempty = gmempty

instance Semigroup PartialOptions where
  (<>) = gmappend

instance Monoid PartialOptions where
  mempty = gmempty

mkPostgresOptions :: PostgresPartialOptions -> Maybe PostgresOptions
mkPostgresOptions PostgresOptions {..} = do
  fHost     <- getLast host
  fPort     <- getLast port
  fUser     <- getLast user
  fPassword <- getLast password
  fDatabase <- getLast database
  pure $ PostgresOptions { host = fHost, port = fPort, user = fUser, password = fPassword, database = fDatabase }

mkOptions :: PartialOptions -> Maybe Options
mkOptions Options {..} = do
  fPostgresOptions <- mkPostgresOptions postgresOptions
  fHttpPort        <- getLast httpPort
  pure $ Options { httpPort = fHttpPort, postgresOptions = fPostgresOptions }
