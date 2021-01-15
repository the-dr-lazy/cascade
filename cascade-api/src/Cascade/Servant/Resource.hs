module Cascade.Servant.Resource
  ( NotFound(..)
  , Ok(..)
  , notFound
  , ok
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
