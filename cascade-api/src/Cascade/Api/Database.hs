{-|
Module      : Cascade.Api.Database
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Api.Database (Database, database) where

import           Cascade.Api.Database.Project        ( ProjectTable )
import           Cascade.Api.Database.User           ( UserTable(..) )
import           Data.Generics.Labels                ( )
import           Database.Beam                       ( DatabaseSettings
                                                     , TableEntity
                                                     , dbModification
                                                     , fieldNamed
                                                     , modifyTableFields
                                                     , tableModification
                                                     , withDbModification
                                                     )
import qualified Database.Beam                      as Beam

-- brittany-disable-next-binding
data Database (f :: Type -> Type) = Database
  { projects :: f (TableEntity ProjectTable)
  , users    :: f (TableEntity UserTable)
  }
  deriving stock Generic
  deriving anyclass (Beam.Database backend)

database :: DatabaseSettings backend Database
database = Beam.defaultDbSettings `withDbModification` dbModification
    { users = modifyTableFields tableModification { emailAddress      = fieldNamed "email_address"
                                                  , encryptedPassword = fieldNamed "encrypted_password"
                                                  , createdAt         = fieldNamed "created_at"
                                                  , updatedAt         = fieldNamed "updated_at"
                                                  }
    }
