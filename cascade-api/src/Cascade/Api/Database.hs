module Cascade.Api.Database
  ( Database
  , database
  ) where

import           Cascade.Api.Database.Project       ( ProjectTable )
import           Data.Generics.Labels           ( )
import           Database.Beam                  ( DatabaseSettings
                                                , TableEntity
                                                )
import qualified Database.Beam                 as Beam

-- brittany-disable-next-binding
data Database (f :: Type -> Type) = Database
  { projects :: f (TableEntity ProjectTable)
  }
  deriving stock Generic
  deriving anyclass (Beam.Database backend)

database :: DatabaseSettings backend Database
database = Beam.defaultDbSettings
