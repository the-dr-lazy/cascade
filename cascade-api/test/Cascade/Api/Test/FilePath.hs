{-|
Module      : Cascade.Api.Test.FilePath
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Test.FilePath
  ( findSqitchConfigFileUpward
  ) where

import           System.Directory
import           System.FilePath

findFileUpward :: (FilePath -> Bool) -> FilePath -> IO (Maybe FilePath)
findFileUpward f top = do
  contents <- getDirectoryContents base
  case find f contents of
    Nothing | base == top -> pure Nothing
            | otherwise   -> findFileUpward f base
    Just path -> return (Just (base </> path))
  where base = takeDirectory top

findSqitchConfigFileUpward :: FilePath -> IO (Maybe FilePath)
findSqitchConfigFileUpward = findFileUpward (== "sqitch.conf")
