{-|
Module      : Cascade.Api.Servant.Response
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Servant.Response
  ( NotFound(..)
  , Ok(..)
  , Created(..)
  , Unprocessable(..)
  , Conflict(..)
  , Forbidden(..)
  , Unauthorized(..)
  , notFound
  , ok
  , created
  ) where

import           Data.Aeson                     ( (.=)
                                                , FromJSON(..)
                                                , ToJSON(..)
                                                , object
                                                )
import           Servant                 hiding ( Unauthorized )

data NotFound = NotFound
  deriving stock Show

instance FromJSON NotFound where
  parseJSON _ = pure NotFound

instance ToJSON NotFound where
  toJSON _ = object ["title" .= ("Not Found" :: Text)]

instance HasStatus NotFound where
  type StatusOf NotFound = 404

notFound :: NotFound
notFound = NotFound

newtype Ok a = Ok a
  deriving stock Show
  deriving newtype (FromJSON, ToJSON)

instance HasStatus (Ok a) where
  type StatusOf (Ok a) = 200

ok :: a -> Ok a
ok = Ok

newtype Created a = Created a
  deriving stock Show
  deriving newtype (FromJSON, ToJSON)

instance HasStatus (Created a) where
  type StatusOf (Created a) = 201

created :: a -> Created a
created = Created

newtype Unprocessable a = Unprocessable a
  deriving stock Show
  deriving newtype (FromJSON, ToJSON)

instance HasStatus (Unprocessable a) where
  type StatusOf (Unprocessable a) = 422

data Conflict = Conflict
  deriving stock Show

instance FromJSON Conflict where
  parseJSON _ = pure Conflict

instance ToJSON Conflict where
  toJSON _ = object ["title" .= ("Conflict" :: Text)]

instance HasStatus Conflict where
  type StatusOf Conflict = 409

data Forbidden = Forbidden
  deriving stock Show

instance FromJSON Forbidden where
  parseJSON _ = pure Forbidden

instance ToJSON Forbidden where
  toJSON _ = object ["title" .= ("Forbidden" :: Text)]

instance HasStatus Forbidden where
  type StatusOf Forbidden = 403

data Unauthorized = Unauthorized
  deriving stock Show

instance FromJSON Unauthorized where
  parseJSON _ = pure Unauthorized

instance ToJSON Unauthorized where
  toJSON _ = object ["title" .= ("Unauthorized" :: Text)]

instance HasStatus Unauthorized where
  type StatusOf Unauthorized = 401
