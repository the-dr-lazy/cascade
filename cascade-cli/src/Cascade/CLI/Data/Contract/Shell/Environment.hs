{-|
Module      : Cascade.CLI.Data.Contract.Shell.Environment
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.CLI.Data.Contract.Shell.Environment
    ( readConfig
    ) where

import qualified Cascade.CLI.Data.Contract.Shell.Environment.Var as Environment.Var
import           Cascade.CLI.Data.Model.Config                   (ConfigP (..))
import qualified Cascade.CLI.Data.Model.Config                   as Config
import           Cascade.Data.Text                               as Text
import           Data.Attoparsec.Text                            (decimal, endOfInput, parseOnly)
import           System.Environment                              (lookupEnv)

readEnvDecimal :: Integral a => String -> IO (Maybe a)
readEnvDecimal envName = do
  value <- lookupEnv envName
  case value of
    Nothing -> pure Nothing
    Just v  -> case parseOnly (decimal <* endOfInput) (Text.pack v) of
      Left  _ -> die <| "Parsing environment variable " <> envName <> " failed!"
      Right a -> pure <| Just a

readPostgresConfig :: IO Config.PostgresPartial
readPostgresConfig = do
  host     <- Last <$> lookupEnv Environment.Var.cascadePostgresHost
  port     <- Last <$> readEnvDecimal Environment.Var.cascadePostgresPort
  user     <- Last <$> lookupEnv Environment.Var.cascadePostgresUser
  password <- Last <$> lookupEnv Environment.Var.cascadePostgresPassword
  database <- Last <$> lookupEnv Environment.Var.cascadePostgresDatabase
  pure Config.Postgres { .. }

readConfig :: IO Config.Partial
readConfig = do
  httpPort <- Last <$> readEnvDecimal Environment.Var.cascadeHttpPort
  postgres <- readPostgresConfig
  pure Config { .. }
