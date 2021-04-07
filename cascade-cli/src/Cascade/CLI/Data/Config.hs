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
import           Control.Lens                        ( (^.)
                                                     , to
                                                     )
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
  { httpPort :: Validate p Word16 FreePort
  , postgres :: PostgresConfigP p
  } deriving stock Generic

type Final = ConfigP 'Final

type Partial = ConfigP 'Partial

deriving stock instance Show Final
deriving via Generically Partial instance Semigroup Partial
deriving via Generically Partial instance Monoid Partial

data Error = BusyHttpPortError
  deriving stock (Show, Eq)

type Errors = NonEmpty Error

finalize :: Partial -> IO (Validation Errors Final)
finalize partial = getCompose do
  httpPort <- Compose <| Validation.maybeToSuccess (BusyHttpPortError :| []) <$> FreePort.mk
    (partial ^. #httpPort . to (fromMaybe Config.Default.httpPort . coerce))

  postgres <-
    PostgresConfig
    <$> pureMaybe Config.Default.postgresHost     (partial ^. #postgres . #host)
    <*> pureMaybe Config.Default.postgresPort     (partial ^. #postgres . #port)
    <*> pureMaybe Config.Default.postgresUser     (partial ^. #postgres . #user)
    <*> pureMaybe Config.Default.postgresPassword (partial ^. #postgres . #password)
    <*> pureMaybe Config.Default.postgresDatabase (partial ^. #postgres . #database)

  pure Config { .. }
