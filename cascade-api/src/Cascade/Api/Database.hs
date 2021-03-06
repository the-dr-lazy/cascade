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

module Cascade.Api.Database
    ( Database
    , database
    ) where

import           Cascade.Api.Database.ProjectTable     ( ProjectTable )
import qualified Cascade.Api.Database.ProjectTable     as ProjectTable
import           Cascade.Api.Database.TaskTable        ( TaskTable (..) )
import           Cascade.Api.Database.UserProjectTable ( UserProjectTable (..) )
import           Cascade.Api.Database.UserTable        ( UserTable (..) )
import qualified Cascade.Api.Database.UserTable        as UserTable
import           Data.Generics.Labels                  ()
import           Database.Beam
    ( DatabaseSettings, TableEntity, dbModification, modifyTableFields, setEntityName,
    tableModification, withDbModification )
import qualified Database.Beam                         as Beam

data Database (f :: Type -> Type) = Database { projects     :: f (TableEntity ProjectTable)
                                             , tasks        :: f (TableEntity TaskTable)
                                             , userProjects :: f (TableEntity UserProjectTable)
                                             , users        :: f (TableEntity UserTable)
                                             }
  deriving stock (Generic)
  deriving anyclass (Beam.Database backend)

database :: DatabaseSettings backend Database
database = Beam.defaultDbSettings `withDbModification` dbModification
  { users        = modifyTableFields tableModification { emailAddress      = "email_address"
                                                       , encryptedPassword = "encrypted_password"
                                                       , createdAt         = "created_at"
                                                       , updatedAt         = "updated_at"
                                                       }
  , userProjects = setEntityName "user_projects" <> modifyTableFields tableModification { userId    = UserTable.PrimaryKey "user_id"
                                                                                        , projectId = ProjectTable.PrimaryKey "project_id"
                                                                                        , createdAt = "created_at"
                                                                                        , updatedAt = "updated_at"
                                                                                        }
  , tasks        = modifyTableFields tableModification { deadlineAt = "deadline_at", projectId = ProjectTable.PrimaryKey "project_id" }
  }
