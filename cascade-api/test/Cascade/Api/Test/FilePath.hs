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
