module Cascade.Data.Chronos.Future
  ( Future
  , ValidationError(..)
  , ValidationErrors
  , pattern Future
  , un
  , mk
  , IsFuture
  )
where

import           Control.Lens.TH                ( makeWrapped )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Chronos                        ( Time
                                                , OffsetDatetime
                                                , offsetDatetimeToTime
                                                )
import           Validation

newtype Future a = Mk
  { un :: a }
  deriving newtype (Show, Eq, FromJSON, ToJSON)

makeWrapped ''Future

pattern Future :: a -> Future a
pattern Future a <- Mk a
{-# COMPLETE Future #-}

data ValidationError
  = IsPast
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

type ValidationErrors = NonEmpty ValidationError

class IsFuture (a :: Type) where
  mk :: a -> Time -> Validation ValidationErrors (Future a)

instance IsFuture OffsetDatetime where
  mk date now = Mk date <$ failureIf isPast IsPast
   where
    isPast :: Bool
    isPast = now > offsetDatetimeToTime date

instance IsFuture Time where
  mk date now = Mk date <$ failureIf isPast IsPast
   where
    isPast :: Bool
    isPast = now > date
