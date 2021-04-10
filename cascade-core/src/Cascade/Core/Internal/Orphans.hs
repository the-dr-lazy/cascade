{-|
Module      : Cascade.Core.Internal.Orphans
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cascade.Core.Internal.Orphans () where

import qualified Chronos
import           Chronos.Types
import qualified Data.Attoparsec.ByteString         as Attoparsec
import qualified Database.Beam                      as Beam
import qualified Database.Beam.Backend              as Beam
import           Database.Beam.Postgres              ( Postgres
                                                     , ResultError(..)
                                                     )
import           Database.Beam.Postgres.Syntax       ( PgValueSyntax
                                                     , defaultPgValueSyntax
                                                     )
import qualified Database.PostgreSQL.Simple.FromField
                                                    as Postgres
import           Database.PostgreSQL.Simple.FromField
                                                     ( typeOid )
import qualified Database.PostgreSQL.Simple.ToField as Postgres
import           Database.PostgreSQL.Simple.TypeInfo.Static
                                                     ( timestamptzOid )

-------------------------------------------------------
-- OffsetDatetime

instance Postgres.ToField OffsetDatetime where
  toField = Postgres.Plain . Postgres.inQuotes . encode
   where
    encode = Chronos.builderUtf8_YmdHMSz OffsetFormatColonAuto (SubsecondPrecisionFixed 6) (DatetimeFormat (Just '-') (Just ' ') (Just ':'))

instance Postgres.FromField OffsetDatetime where
  fromField f
    | typeOid f /= timestamptzOid = return $ Postgres.returnError Incompatible f ""
    | otherwise = maybe (Postgres.returnError UnexpectedNull f "") (maybe (Postgres.returnError ConversionFailed f "") return <$> decode)
   where
    decode = rightToMaybe . Attoparsec.parseOnly (parser <* Attoparsec.endOfInput)
    parser = Chronos.parserUtf8_YmdHMSz OffsetFormatColonAuto (DatetimeFormat (Just '-') (Just ' ') (Just ':'))

instance Beam.FromBackendRow Postgres OffsetDatetime

instance Beam.HasSqlValueSyntax PgValueSyntax OffsetDatetime where
  sqlValueSyntax = defaultPgValueSyntax

instance Beam.HasSqlEqualityCheck Postgres OffsetDatetime

-------------------------------------------------------
-- Time

instance Postgres.ToField Time where
  toField = Postgres.toField . Chronos.timeToOffsetDatetime (Chronos.Offset 0)

instance Postgres.FromField Time where
  fromField f = fmap Chronos.offsetDatetimeToTime . Postgres.fromField f

instance Beam.FromBackendRow Postgres Time

instance Beam.HasSqlValueSyntax PgValueSyntax Time where
  sqlValueSyntax = defaultPgValueSyntax

instance Beam.HasSqlEqualityCheck Postgres Time
