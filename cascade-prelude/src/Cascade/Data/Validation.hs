{-|
Module      : Cascade.Data.Validation
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

{-# LANGUAGE ViewPatterns #-}

module Cascade.Data.Validation
    ( Errors
    , Phase (..)
    , Validate
    , module Validation
    , parseRecord
    ) where

import           GHC.Generics
import           Validation

data Phase = Parser | Raw | Error | Parsed

type family Errors (raw :: Type) (parsed :: Type) :: Type

type instance Errors (a 'Raw) (a 'Parsed) = a 'Error
type instance Errors (Maybe raw) (Maybe parsed) = Errors raw parsed

type family Validate (p :: Phase) (raw :: Type) (parsed :: Type) where
  Validate 'Parser  (Maybe raw) (Maybe parsed) = Validate 'Parser raw parsed
  Validate 'Parser  raw         parsed         = raw -> Validation (Errors raw parsed) parsed
  Validate 'Raw     raw         _              = raw
  Validate 'Error   raw         parsed         = Maybe (Errors raw parsed)
  Validate 'Parsed  _           parsed         = parsed

parseRecord :: forall a
             . Generic (a 'Raw)
            => Generic (a 'Parsed)
            => Generic (a 'Error)
            => Generic (a 'Parser)
            => GenericParseRecord (Rep (a 'Parser)) (Rep (a 'Raw)) (Rep (a 'Error)) (Rep (a 'Parsed))
            => a 'Parser
            -> a 'Raw
            -> Validation (a 'Error) (a 'Parsed)
parseRecord (from -> parser) (from -> raw) = bimap to to <| genericParseRecord parser raw

class GenericParseRecord parser raw error parsed where
  genericParseRecord :: parser p -> raw p -> Validation (error p) (parsed p)

instance GenericParseRecord parser raw error parsed => GenericParseRecord (M1 i m parser) (M1 i m raw) (M1 i m error) (M1 i m parsed) where
  genericParseRecord (M1 f) (M1 x) = bimap M1 M1 (genericParseRecord f x)

instance GenericParseRecord (Rec0 (raw -> Validation error parsed)) (Rec0 (Maybe raw)) (Rec0 (Maybe error)) (Rec0 (Maybe parsed)) where
  genericParseRecord (K1 parser) (K1 mraw) = bimap (K1 . Just) K1 <| case mraw of
    Nothing  -> Success Nothing
    Just raw -> Just <$> parser raw

instance GenericParseRecord (Rec0 (raw -> Validation error parsed)) (Rec0 raw) (Rec0 (Maybe error)) (Rec0 parsed) where
  genericParseRecord (K1 f) (K1 x) = bimap (K1 . Just) K1 (f x)

instance ( GenericParseRecord parser1 raw1 error1 parsed1
         , GenericParseRecord parser2 raw2 error2 parsed2
         , GenericParsedToError parsed1 error1
         , GenericParsedToError parsed2 error2
         ) => GenericParseRecord (parser1 :*: parser2) (raw1 :*: raw2) (error1 :*: error2) (parsed1 :*: parsed2) where
  genericParseRecord (lparser :*: rparser) (lraw :*: rraw) = case (genericParseRecord lparser lraw, genericParseRecord rparser rraw) of
    (Success parsed1, Success parsed2) -> Success (parsed1 :*: parsed2)
    (Failure error1 , Failure error2 ) -> Failure (error1 :*: error2)
    (Failure error1 , Success parsed2) -> Failure (error1 :*: genericParsedToError parsed2)
    (Success parsed1, Failure error2 ) -> Failure (genericParsedToError parsed1 :*: error2)

class GenericParsedToError parsed error where
  genericParsedToError :: parsed p -> error p

instance GenericParsedToError parsed error => GenericParsedToError (S1 m parsed) (S1 m error) where
  genericParsedToError (M1 parsed) = M1 (genericParsedToError parsed)

instance GenericParsedToError (Rec0 parsed) (Rec0 (Maybe error)) where
  genericParsedToError (K1 _) = K1 Nothing

instance (GenericParsedToError parsed1 error1, GenericParsedToError parsed2 error2) => GenericParsedToError (parsed1 :*: parsed2) (error1 :*: error2) where
  genericParsedToError (parsed1 :*: parsed2) = genericParsedToError parsed1 :*: genericParsedToError parsed2
