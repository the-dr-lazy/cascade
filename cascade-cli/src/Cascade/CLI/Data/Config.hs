{-|
Module      : Cascade.CLI.Data.Config
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.CLI.Data.Config (ConfigP(..), PostgresConfigP(..), Partial, PostgresPartial, Final, PostgresFinal, finalize, Errors) where

import qualified Cascade.CLI.Data.Config.Default    as Config.Default
import           Cascade.CLI.Data.Model.FreePort     ( FreePort )
import qualified Cascade.CLI.Data.Model.FreePort    as FreePort
import           Cascade.Data.Maybe                  ( pureMaybe )
import           Data.Generics.Labels                ( )
import           Generic.Data                        ( Generically(..) )
import           Validation                          ( Validation )
import qualified Validation

data Phase = Partial | Final

type family Validate (p :: Phase) (raw :: Type) (parsed :: Type) where
  Validate 'Partial raw _ = Last raw
  Validate 'Final _ parsed = parsed

-- brittany-disable-next-binding
data PostgresConfigP (p :: Phase) = PostgresConfig
  { host     :: Validate p String String
  , port     :: Validate p Word16 Word16
  , user     :: Validate p String String
  , password :: Validate p String String
  , database :: Validate p String String
  } deriving stock Generic

type PostgresFinal = PostgresConfigP 'Final

type PostgresPartial = PostgresConfigP 'Partial

deriving stock instance Show PostgresFinal
deriving via Generically PostgresPartial instance Semigroup PostgresPartial
deriving via Generically PostgresPartial instance Monoid PostgresPartial

-- brittany-disable-next-binding
data ConfigP (p :: Phase) = Config
  { httpPort        :: Validate p Word16 FreePort
  , postgresConfig :: PostgresConfigP p
  } deriving stock Generic

type Final = ConfigP 'Final

type Partial = ConfigP 'Partial

deriving stock instance Show Final
deriving via Generically Partial instance Semigroup Partial
deriving via Generically Partial instance Monoid Partial

data Error = BusyHttpPortError
  deriving stock (Show, Eq)

type Errors = NonEmpty Error

finalizePostgres :: PostgresPartial -> IO (Validation Errors PostgresFinal)
finalizePostgres PostgresConfig {..} =
  let validateHost     = pureMaybe Config.Default.postgresHost host
      validatePort     = pureMaybe Config.Default.postgresPort port
      validateUser     = pureMaybe Config.Default.postgresUser user
      validatePassword = pureMaybe Config.Default.postgresPassword password
      validateDatabase = pureMaybe Config.Default.postgresDatabase database
  in  pure <| PostgresConfig <$> validateHost <*> validatePort <*> validateUser <*> validatePassword <*> validateDatabase

finalize :: Partial -> IO (Validation Errors Final)
finalize Config {..} = do
  validateHttpPort       <- Validation.maybeToSuccess (BusyHttpPortError :| []) <$> FreePort.mk httpPort
  validatePostgresConfig <- finalizePostgres postgresConfig
  pure <| Config <$> validateHttpPort <*> validatePostgresConfig
