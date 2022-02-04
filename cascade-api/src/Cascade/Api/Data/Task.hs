{-|
Module      : Cascade.Api.Data.Task
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2022 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

{-# LANGUAGE UndecidableInstances #-}

module Cascade.Api.Data.Task
    ( Creatable (..)
    , Id
    , Readable (..)
    , Task
    , Updatable (..)
    , parseRawCreatable
    , parseRawUpdatable
    ) where

import qualified Cascade.Api.Data.Aeson.RecordErrorFormat as Aeson
import qualified Cascade.Api.Data.Id                      as Data
import           Cascade.Api.Data.OffsetDatetime
    ( FormattedOffsetDatetime, unFormattedOffsetDatetime )
import           Cascade.Api.Data.OffsetDatetime.Deadline ( Deadline )
import qualified Cascade.Api.Data.OffsetDatetime.Deadline as Deadline
import qualified Cascade.Api.Data.Project                 as Project
import           Cascade.Api.Data.Text.Title              ( Title )
import qualified Cascade.Api.Data.Text.Title              as Title
import           Cascade.Api.Effect.Time                  ( TimeL )
import qualified Cascade.Api.Effect.Time                  as Time
import           Cascade.Data.Validation                  ( Validate, Validation )
import qualified Cascade.Data.Validation                  as Validation
import           Data.Aeson                               ( FromJSON (..), ToJSON (..) )
import           Polysemy                                 ( Member, Sem )

data Task

type Id = Data.Id Task

data Readable = Readable { id         :: Id
                         , title      :: Title
                         , deadlineAt :: FormattedOffsetDatetime
                         , projectId  :: Project.Id
                         }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data Creatable p = Creatable { title      :: Validate p Text Title
                             , deadlineAt :: Validate p FormattedOffsetDatetime Deadline
                             }
  deriving stock (Generic)

deriving stock instance Show (Creatable 'Validation.Raw)
deriving stock instance Eq   (Creatable 'Validation.Raw)
deriving anyclass instance ToJSON   (Creatable 'Validation.Raw)
deriving anyclass instance FromJSON (Creatable 'Validation.Raw)

deriving stock instance Show (Creatable 'Validation.Error)
deriving via Aeson.RecordErrorFormat (Creatable 'Validation.Error) instance ToJSON   (Creatable 'Validation.Error)
deriving via Aeson.RecordErrorFormat (Creatable 'Validation.Error) instance FromJSON (Creatable 'Validation.Error)


parseRawCreatable :: Member TimeL r
                  => Creatable 'Validation.Raw
                  -> Sem r (Validation (Creatable 'Validation.Error) (Creatable 'Validation.Parsed))
parseRawCreatable raw = do
  now <- Time.now
  pure <| Validation.parseRecord Creatable { title = Title.parse, deadlineAt = Deadline.parse now . unFormattedOffsetDatetime } raw

data Updatable p = Updatable { title :: Validate p (Maybe Text) (Maybe Title)
                             , deadlineAt :: Validate p (Maybe FormattedOffsetDatetime) (Maybe Deadline)
                             }
  deriving stock (Generic)

deriving stock    instance Show     (Updatable 'Validation.Raw)
deriving stock    instance Eq       (Updatable 'Validation.Raw)
deriving anyclass instance ToJSON   (Updatable 'Validation.Raw)
deriving anyclass instance FromJSON (Updatable 'Validation.Raw)

deriving stock instance Show (Updatable 'Validation.Error)
deriving via Aeson.RecordErrorFormat (Updatable 'Validation.Error) instance ToJSON   (Updatable 'Validation.Error)
deriving via Aeson.RecordErrorFormat (Updatable 'Validation.Error) instance FromJSON (Updatable 'Validation.Error)

parseRawUpdatable :: Member TimeL r
                  => Updatable 'Validation.Raw
                  -> Sem r (Validation (Updatable 'Validation.Error) (Updatable 'Validation.Parsed))
parseRawUpdatable raw = do
  now <- Time.now
  pure <| Validation.parseRecord Updatable { title = Title.parse, deadlineAt = Deadline.parse now . unFormattedOffsetDatetime } raw
