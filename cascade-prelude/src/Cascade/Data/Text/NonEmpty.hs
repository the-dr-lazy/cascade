{-|
Module      : Cascade.Data.Text.NonEmpty
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

{-# LANGUAGE DeriveDataTypeable #-}

module Cascade.Data.Text.NonEmpty
    ( NonEmpty
    , mk
    , pattern NonEmpty
    , un
    ) where


import           Control.Lens.TH (makeWrapped)
import           Data.Aeson      (FromJSON, ToJSON)
import qualified Data.Text       as Text
import           Prelude         hiding (NonEmpty)

newtype NonEmpty
  = Mk { un :: Text }
  deriving newtype (Eq, FromJSON, Show, ToJSON)

makeWrapped ''NonEmpty

pattern NonEmpty :: Text -> NonEmpty
pattern NonEmpty a <- Mk a
{-# COMPLETE NonEmpty #-}

mk :: Text -> Maybe NonEmpty
mk input = if Text.null input then Nothing else Just $ Mk input
