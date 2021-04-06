{-|
Module      : Cascade.CLI.Data.Config.Default
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.CLI.Data.Config.Default (httpPort, postgresHost, postgresPort, postgresUser, postgresPassword, postgresDatabase) where

httpPort :: Int
httpPort = 3141

postgresHost :: String
postgresHost = "localhost"

postgresPort :: Word16
postgresPort = 5432

postgresUser :: String
postgresUser = "cascade"

postgresPassword :: String
postgresPassword = ""

postgresDatabase :: String
postgresDatabase = "cascade-api"
