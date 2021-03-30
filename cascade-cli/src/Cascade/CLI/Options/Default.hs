{-|
Module      : Cascade.CLI.Options.Default
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.CLI.Options.Default (defaultPartialOptions) where

import           Cascade.CLI.Options                 ( OptionsP(..)
                                                     , PartialOptions
                                                     , PostgresOptionsP(..)
                                                     , PostgresPartialOptions
                                                     )

defaultPostgresPartialOptions :: PostgresPartialOptions
defaultPostgresPartialOptions =
  PostgresOptions { host = pure "localhost", port = pure 5432, user = pure "cascade", password = pure "", database = pure "cascad-api" }

defaultPartialOptions :: PartialOptions
defaultPartialOptions = Options { httpPort = pure 3141, postgresOptions = defaultPostgresPartialOptions }
