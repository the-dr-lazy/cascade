{-|
Module      : Cascade.Logger.Formatting
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Logger.Formatting
    ( color
    , square
    ) where

import qualified Data.Text           as Text
import           System.Console.ANSI
    ( Color, ColorIntensity (Vivid), ConsoleLayer (Foreground), SGR (..), setSGRCode )

square :: Text -> Text
square t = "[" <> t <> "] "

color :: Color -> Text -> Text
color c txt = Text.pack (setSGRCode [SetColor Foreground Vivid c]) <> txt <> Text.pack (setSGRCode [Reset])
