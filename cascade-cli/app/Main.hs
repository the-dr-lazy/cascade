{-|
Module      : Main
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Main (main) where

import           Cascade.CLI                         ( cascadeCli )
import           System.IO                           ( hSetEncoding
                                                     , utf8
                                                     )

main :: IO ()
main = do
  hSetEncoding stdout utf8 >> cascadeCli
