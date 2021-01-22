module Cascade.Api.Servant.Response
  ( NotFound(..)
  , Ok(..)
  , Created(..)
  , notFound
  , ok
  , created
  ) where

import           Data.Aeson                     ( (.=)
                                                , FromJSON(..)
                                                , ToJSON(..)
                                                , object
                                                )
import           Servant

data NotFound = NotFound

instance FromJSON NotFound where
  parseJSON _ = pure NotFound

instance ToJSON NotFound where
  toJSON _ = object ["title" .= ("Not Found" :: Text)]

instance HasStatus NotFound where
  type StatusOf NotFound = 404

notFound :: NotFound
notFound = NotFound

newtype Ok a = Ok a deriving newtype (FromJSON, ToJSON)

instance HasStatus (Ok a) where
  type StatusOf (Ok a) = 200

ok :: a -> Ok a
ok = Ok

newtype Created a = Created a deriving newtype (FromJSON, ToJSON)

instance HasStatus (Created a) where
  type StatusOf (Created a) = 201

created :: a -> Created a
created = Created
