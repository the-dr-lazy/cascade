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

module Cascade.CLI.Data.Config (ConfigP(..), PostgresConfigP(..), Partial, PostgresPartial, Final, PostgresFinal, finalize, def) where

import           Cascade.CLI.Data.HttpPort           ( HttpPort )
import qualified Cascade.CLI.Data.HttpPort          as HttpPort
import           Cascade.Data.Validation             ( Validate
                                                     , Validation
                                                     )
import qualified Cascade.Data.Validation            as Validation
import           Control.Lens                        ( (^.)
                                                     , to
                                                     )
import           Data.Generics.Labels                ( )
import           Generic.Data                        ( Generically(..) )

-- brittany-disable-next-binding
data PostgresConfigP p = PostgresConfig
  { host     :: Validate p (Last String) String
  , port     :: Validate p (Last Word16) Word16
  , user     :: Validate p (Last String) String
  , password :: Validate p (Last String) String
  , database :: Validate p (Last String) String
  } deriving stock Generic

type PostgresFinal = PostgresConfigP 'Validation.Parsed

type PostgresPartial = PostgresConfigP 'Validation.Raw

deriving stock instance Show (PostgresConfigP 'Validation.Error)
deriving via Generically PostgresPartial instance Semigroup PostgresPartial
deriving via Generically PostgresPartial instance Monoid PostgresPartial

-- brittany-disable-next-binding
data ConfigP p = Config
  { httpPort       :: Validate p (Last Int) HttpPort
  , postgresConfig :: Validate p (PostgresConfigP 'Validation.Raw) (PostgresConfigP 'Validation.Parsed)
  } deriving stock Generic

type Final = ConfigP 'Validation.Parsed

type Partial = ConfigP 'Validation.Raw

deriving stock instance Show (ConfigP 'Validation.Error)
deriving via Generically Partial instance Semigroup Partial
deriving via Generically Partial instance Monoid Partial

type instance Validation.Errors (Last String) String = ()
type instance Validation.Errors (Last Word16) Word16 = ()

def :: Final
def = Config { httpPort = HttpPort.def, postgresConfig = defPostgres }
 where
  defPostgres :: PostgresFinal
  defPostgres = PostgresConfig { host = "localhost", port = 5432, user = "cascade", password = "", database = "cascad-api" }

finalizePostgres :: PostgresPartial -> IO (Validation (PostgresConfigP 'Validation.Error) PostgresFinal)
finalizePostgres raw =
  pure
    <| Validation.parseRecord
         PostgresConfig { host     = const . Validation.Success <| fromMaybe (host . postgresConfig <| def) (getLast (raw ^. #host))
                        , port     = const . Validation.Success <| fromMaybe (port . postgresConfig <| def) (getLast (raw ^. #port))
                        , user     = const . Validation.Success <| fromMaybe (user . postgresConfig <| def) (getLast (raw ^. #user))
                        , password = const . Validation.Success <| fromMaybe (password . postgresConfig <| def) (getLast (raw ^. #password))
                        , database = const . Validation.Success <| fromMaybe (database . postgresConfig <| def) (getLast (raw ^. #database))
                        }
         raw

finalize :: Partial -> IO (Validation (ConfigP 'Validation.Error) Final)
finalize raw = do
  parseHttpPort       <- HttpPort.mk <| fromMaybe (def ^. #httpPort . to HttpPort.un) (getLast (httpPort raw))
  parsePostgresConfig <- finalizePostgres (raw ^. #postgresConfig)
  pure <| Validation.parseRecord (Config { httpPort = const parseHttpPort, postgresConfig = const parsePostgresConfig }) raw
