module Cascade.CLI.Environment (readEnvPartialOptions) where

import           Cascade.CLI.Data.Config             ( ConfigP(..)
                                                     , PartialConfig
                                                     , PostgresConfigP(..)
                                                     , PostgresPartialConfig
                                                     )
import           Cascade.Data.Text                  as T
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
    Just v  -> case parseOnly (decimal <* endOfInput) (T.pack v) of
      Left  _ -> pure Nothing
      Right a -> pure <| Just a

readEnvPostgresPartialOptions :: IO PostgresPartialConfig
readEnvPostgresPartialOptions = do
  host     <- Last <$> lookupEnv "CASCADE_POSTGRES_HOST"
  port     <- Last <$> readEnvDecimal "CASCADE_POSTGRES_PORT"
  user     <- Last <$> lookupEnv "CASCADE_POSTGRES_USER"
  password <- Last <$> lookupEnv "CASCADE_POSTGRES_PASSWORD"
  database <- Last <$> lookupEnv "CASCADE_POSTGRES_DATABASE"
  pure PostgresConfig { .. }

readEnvPartialOptions :: IO PartialConfig
readEnvPartialOptions = do
  httpPort       <- Last <$> readEnvDecimal "CASCADE_HTTP_PORT"
  postgresConfig <- readEnvPostgresPartialOptions
  pure Config { .. }
