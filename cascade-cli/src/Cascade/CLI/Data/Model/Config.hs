{-|
Module      : Cascade.CLI.Data.Model.Config
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.CLI.Data.Model.Config
  ( ConfigP(..)
  , PostgresP(..)
  , Partial
  , PostgresPartial
  , Final
  , PostgresFinal
  , finalize
  , Errors
  , logErrors
  ) where

import qualified Cascade.CLI.Data.Model.Config.Default
                                                    as Config.Default
import           Cascade.CLI.Data.Model.FreePort     ( FreePort )
import qualified Cascade.CLI.Data.Model.FreePort    as FreePort
import           Cascade.Colog.Message               ( Message
                                                     , log
                                                     )
import           Cascade.Control.Applicative         ( pureMaybe )
import qualified Cascade.Data.Maybe                 as Maybe
import           Colog                               ( Severity(..)
                                                     , WithLog
                                                     )
import           Control.Lens                        ( (^.)
                                                     , non
                                                     , to
                                                     )
import           Data.Generics.Labels                ( )
import           Generic.Data                        ( Generically(..) )
import           Validation                          ( Validation )

data Phase = Partial | Final

type family Finalize (p :: Phase) (raw :: Type) (parsed :: Type) where
  Finalize 'Partial raw _ = Last raw
  Finalize 'Final _ parsed = parsed

type Finalize' p t = Finalize p t t

-- brittany-disable-next-binding
data PostgresP (p :: Phase) = Postgres
  { host     :: Finalize' p String
  , port     :: Finalize' p Word16
  , user     :: Finalize' p String
  , password :: Finalize' p String
  , database :: Finalize' p String
  } deriving stock Generic

type PostgresFinal = PostgresP 'Final

type PostgresPartial = PostgresP 'Partial

deriving stock instance Show PostgresFinal
deriving via Generically PostgresPartial instance Semigroup PostgresPartial
deriving via Generically PostgresPartial instance Monoid PostgresPartial

-- brittany-disable-next-binding
data ConfigP (p :: Phase) = Config
  { httpPort :: Finalize p Word16 FreePort
  , postgres :: PostgresP p
  } deriving stock Generic

type Final = ConfigP 'Final

type Partial = ConfigP 'Partial

deriving stock instance Show Final
deriving via Generically Partial instance Semigroup Partial
deriving via Generically Partial instance Monoid Partial

data Error = BusyHttpPortError Word16
  deriving stock (Show, Eq)

type Errors = NonEmpty Error

prettyPrintError :: Error -> Text
prettyPrintError (BusyHttpPortError port) = "Port " <> show port <> " is busy, try another port"

logErrors :: WithLog env Message m => MonadIO m => Errors -> m ()
logErrors = mapM_ <| log Error . prettyPrintError

finalize :: Partial -> IO (Validation Errors Final)
finalize partial = getCompose do
  let port = partial ^. #httpPort . to coerce . non Config.Default.httpPort
  httpPort <- Compose . fmap (Maybe.toSuccess (BusyHttpPortError port)) . FreePort.mk <| port

  postgres <-
    Postgres
    <$> pureMaybe Config.Default.postgresHost     (partial ^. #postgres . #host)
    <*> pureMaybe Config.Default.postgresPort     (partial ^. #postgres . #port)
    <*> pureMaybe Config.Default.postgresUser     (partial ^. #postgres . #user)
    <*> pureMaybe Config.Default.postgresPassword (partial ^. #postgres . #password)
    <*> pureMaybe Config.Default.postgresDatabase (partial ^. #postgres . #database)

  pure Config { .. }
