module Cascade.Api.Data.Id
  ( Id(..)
  ) where

import           Control.Lens.TH                ( makeWrapped )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Servant.API                    ( FromHttpApiData
                                                , ToHttpApiData
                                                )

newtype Id (entity :: Type) = Id
  { unId :: UUID }
  deriving stock Generic
  deriving newtype (Show, Eq, Ord, FromHttpApiData, ToHttpApiData, FromJSON, ToJSON)

makeWrapped ''Id
