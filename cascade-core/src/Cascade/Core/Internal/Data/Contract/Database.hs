{-|
Module      : Cascade.Core.Internal.Data.Contract.Database
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Core.Internal.Data.Contract.Database (Database, database) where

import           Cascade.Core.Internal.Data.Contract.Database.ProjectTable
                                                     ( ProjectTable )
import qualified Cascade.Core.Internal.Data.Contract.Database.ProjectTable
                                                    as ProjectTable
import           Cascade.Core.Internal.Data.Contract.Database.TaskTable
                                                     ( TaskTable(..) )
import qualified Cascade.Core.Internal.Data.Contract.Database.TaskTable
                                                    as TaskTable
import           Cascade.Core.Internal.Data.Contract.Database.UserProjectTable
                                                     ( UserProjectTable(..) )
import           Cascade.Core.Internal.Data.Contract.Database.UserTable
                                                     ( UserTable(..) )
import qualified Cascade.Core.Internal.Data.Contract.Database.UserTable
                                                    as UserTable
import           Cascade.Core.Internal.Orphans       ( )
import           Data.Generics.Labels                ( )
import           Database.Beam                       ( DatabaseSettings
                                                     , TableEntity
                                                     , dbModification
                                                     , modifyTableFields
                                                     , setEntityName
                                                     , tableModification
                                                     , withDbModification
                                                     )
import qualified Database.Beam                      as Beam

-- brittany-disable-next-binding
data Database (f :: Type -> Type) = Database
  { projects     :: f (TableEntity ProjectTable)
  , tasks        :: f (TableEntity TaskTable)
  , userProjects :: f (TableEntity UserProjectTable)
  , users        :: f (TableEntity UserTable)
  }
  deriving stock Generic
  deriving anyclass (Beam.Database backend)

database :: DatabaseSettings backend Database
database = Beam.defaultDbSettings `withDbModification` dbModification
  { users        = modifyTableFields tableModification { emailAddress       = "email_address"
                                                       , currentWorkingTask = TaskTable.PrimaryKey "current_working_task"
                                                       , createdAt          = "created_at"
                                                       , updatedAt          = "updated_at"
                                                       }
  , userProjects = setEntityName "user_projects" <> modifyTableFields tableModification { userId    = UserTable.PrimaryKey "user_id"
                                                                                        , projectId = ProjectTable.PrimaryKey "project_id"
                                                                                        , createdAt = "created_at"
                                                                                        , updatedAt = "updated_at"
                                                                                        }
  , tasks        = modifyTableFields tableModification { deadlineAt = "deadline_at", projectId = ProjectTable.PrimaryKey "project_id" }
  }
