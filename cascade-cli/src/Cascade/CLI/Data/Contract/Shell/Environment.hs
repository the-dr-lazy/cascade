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

module Cascade.CLI.Data.Contract.Shell.Environment (readConfig) where

import qualified Cascade.CLI.Data.Model.Config      as Config
import           Cascade.CLI.Data.Model.Config       ( ConfigP(..) )
import           Cascade.Data.Text                  as Text
import           Data.Attoparsec.Text                ( decimal
                                                     , endOfInput
                                                     , parseOnly
                                                     )
import           System.Environment

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
  host     <- Last <$> lookupEnv "CASCADE_POSTGRES_HOST"
  port     <- Last <$> readEnvDecimal "CASCADE_POSTGRES_PORT"
  user     <- Last <$> lookupEnv "CASCADE_POSTGRES_USER"
  password <- Last <$> lookupEnv "CASCADE_POSTGRES_PASSWORD"
  database <- Last <$> lookupEnv "CASCADE_POSTGRES_DATABASE"
  pure Config.Postgres { .. }

readConfig :: IO Config.Partial
readConfig = do
  httpPort <- Last <$> readEnvDecimal "CASCADE_HTTP_PORT"
  postgres <- readPostgresConfig
  pure Config { .. }
