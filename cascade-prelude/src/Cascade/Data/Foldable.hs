{-|
Module      : Cascade.Data.Foldable
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Data.Foldable (defaulting) where

defaulting :: Foldable t => b -> (t a -> b) -> t a -> b
defaulting def f ta | null ta   = def
                    | otherwise = f ta
