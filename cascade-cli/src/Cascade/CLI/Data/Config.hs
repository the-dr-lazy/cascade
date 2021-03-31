module Cascade.CLI.Data.Config
  ( ConfigP(..)
  , PostgresConfigP(..)
  , PartialConfig
  , PostgresPartialConfig
  , Config
  , PostgresConfig
  , finalise
  , defaultPartialConfig
  , defaultPostgresPartialConfig
  ) where

import           Generic.Data                        ( Generically(..) )


data Phase = Partial | Final

infixl 3 :-
type family phase :- field where
  'Partial :- field = Last field
  'Final :- field = field

-- brittany-disable-next-binding
data PostgresConfigP (p :: Phase) = PostgresConfig
  { host     :: p :- String
  , port     :: p :- Word16
  , user     :: p :- String
  , password :: p :- String
  , database :: p :- String
  } deriving stock Generic

type PostgresPartialConfig = PostgresConfigP 'Partial

type PostgresConfig = PostgresConfigP 'Final

-- brittany-disable-next-binding
data ConfigP (p :: Phase) = Config
  { httpPort        :: p :- Int
  , postgresConfig :: PostgresConfigP p
  } deriving stock Generic

type PartialConfig = ConfigP 'Partial

type Config = ConfigP 'Final

deriving via Generically PostgresPartialConfig instance Semigroup PostgresPartialConfig
deriving via Generically PostgresPartialConfig instance Monoid PostgresPartialConfig
deriving via Generically PartialConfig instance Semigroup PartialConfig
deriving via Generically PartialConfig instance Monoid PartialConfig

defaultPostgresPartialConfig :: PostgresPartialConfig
defaultPostgresPartialConfig =
  PostgresConfig { host = pure "localhost", port = pure 5432, user = pure "cascade", password = pure "", database = pure "cascad-api" }

defaultPartialConfig :: PartialConfig
defaultPartialConfig = Config { httpPort = pure 3141, postgresConfig = defaultPostgresPartialConfig }

finalisePostgres :: PostgresPartialConfig -> Maybe PostgresConfig
finalisePostgres PostgresConfig {..} =
  PostgresConfig <$> getLast host <*> getLast port <*> getLast user <*> getLast password <*> getLast database

finalise :: PartialConfig -> Maybe Config
finalise Config {..} = Config <$> getLast httpPort <*> finalisePostgres postgresConfig
