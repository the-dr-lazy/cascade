{-|
Module      : Cascade.Data.Chronos.Future
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Data.Chronos.Future (Future, pattern Future, un, mk) where

import           Control.Lens.TH                     ( makeWrapped )
import           Data.Aeson                          ( FromJSON
                                                     , ToJSON
                                                     )
import           Chronos                             ( Time
                                                     , OffsetDatetime
                                                     , offsetDatetimeToTime
                                                     )

newtype Future a = Mk
  { un :: a }
  deriving stock Show
  deriving newtype (Eq, FromJSON, ToJSON)

makeWrapped ''Future

pattern Future :: a -> Future a
pattern Future a <- Mk a
{-# COMPLETE Future #-}

class IsFuture (a :: Type) where
  mk :: a -> Time -> Maybe (Future a)

instance IsFuture OffsetDatetime where
  mk date now = if isPast then Nothing else Just $ Mk date
   where
    isPast :: Bool
    isPast = now > offsetDatetimeToTime date

instance IsFuture Time where
  mk date now = if isPast then Nothing else Just $ Mk date
   where
    isPast :: Bool
    isPast = now > date
